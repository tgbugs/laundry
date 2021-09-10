#lang racket/base
(require racket/pretty)
(require brag/support
         "lex-abbrev.rkt"
         (only-in racket/string string-suffix? string-trim string-split)
         (only-in racket/port input-port-append)
         (only-in racket/list cons? last)
         (for-syntax racket/base
                     #;
                     syntax/parse
                     (only-in racket/list combinations permutations)
                     ))
(provide laundry-make-tokenizer
         #;
         cumulative-offset
         table-make-tokenizer
         paragraph-make-tokenizer
         ;heading-make-tokenizer
         bind-runtime-todo-keywords

         #;
         find-last
         get-block-type

         (rename-out [debug laundry-tokenizer-debug]
                     [final-port laundry-final-port])

         #; ; colorer needs its own
         token-stop-for-paragraph

         ;heading
         ;hyperlink
         ;hyperlink-ab
         ;comment-element
         ;drawer-ish

         ;paragraph
         #|
         markup-*
         markup-/
         markup-_
         markup-+
         markup-=
         markup-~
         |#
         )

(define debug (make-parameter #f))

(define (find-last char str)
  ; we don't want to do string reverse because it is slow we just want
  ; to find the first char going backward, string-ref backward seems
  ; like it is ok if we really want this to go fast maybe could chunk
  ; bytes into cacheline size and search for a sub-offset within that?
  #;
  (println "find-last searching ...")
  ; FIXME this fails if the pattern we are looking for isn't actually present
  (letrec ([search (λ (look-at)
                     #;
                     (println (list 'find-last: (string-ref str look-at) look-at))
                     (if (eq? (string-ref str look-at) char)
                         (values (substring str 0 look-at) (sub1 look-at))
                         (search (sub1 look-at))))])
    (search (sub1 (string-length str)))))

;(regexp-match #rx"^\n#\\+(begin|BEGIN)_([^ \t\n]+)[ \t\n]" "\n#+begin_:\n#+end_:\n")

(define (get-block-type lexeme)
  ; TODO use the actual block line parser for this once we have it
  (define matched (regexp-match #rx"^\n(?:[ \t]*)#\\+(?:begin|BEGIN)_([^ \t\n]+)(?:[ \t]|$)" lexeme)) ; from/stop-before means we never actually see \n
  (if matched
      (last matched)
      ; have to add the string because apparently racket-mode still loses the context
      (error "get-block-type:" lexeme)))

(define (set-port-position! input-port offset #:back-line [back-line #f])
  "do it in string chars not in bytes"
  (let-values ([(l c p) (port-next-location input-port)])
    (set-port-next-location!
     input-port
     (if back-line (sub1 l) l)
     (if back-line #f (- c offset))
     (- p offset)
     ))
  )

(define (token-back-1 TOKEN lexeme input-port)
  ; FIXME multi-byte case
  (file-position input-port (sub1 (file-position input-port)))
  (let-values ([(l c p) (port-next-location input-port)])
    (set-port-next-location!
     input-port
     l ; TODO newline case
     (and c (sub1 c)) ; FIXME -1 is going to be a problem for =x=\n
     (and p (sub1 p))))
  (token TOKEN (substring lexeme 0 (sub1 (string-length lexeme)))))

(define (token-stop-before TOKEN TOKEN-EOF lexeme char input-port start-pos #:eof [eof-ok #t])
  "Encapsulates the machinery needed to correctly reset the location of input-port when
using from/stop-before where the stop-before pattern contains multiple charachters."
  (when (debug)
    (print (list 'token-stop-before 'lexeme-original lexeme)))
  (if (and eof-ok (eq? (peek-char input-port) eof))
      (token TOKEN-EOF lexeme)
      (let*-values ([(lexeme-correct offset) (find-last char lexeme)]
                    [(token-correct position-correct)
                     (values (token TOKEN lexeme-correct)
                             (+ (position-offset start-pos)
                                offset))]
                    #;
                    [(this-offset) (- (add1 (file-position input-port)) position-correct)])
        (when (debug)
          (println (list 'ooooooo lexeme-correct offset (position-offset start-pos) position-correct)))
        (if (or (file-stream-port? input-port)
                (string-port? input-port)) ; cooperate with laundry lexer first-port
            (begin
              (when (debug)
                (println (list 'p0: (file-position input-port)))
                (println (list 'pnl0: (let-values ([(l c p) (port-next-location input-port)]) (list l c p)))))
              (let-values ([(l c p) (port-next-location input-port)])
                ; TODO we need to transition from file-position over to port-next-location for this probably
                (set-port-next-location! ; FIXME TODO this might be our culprit yep!
                 input-port
                 l #;
                 (if (eq? char #\newline) (sub1 l) l)
                 (if (eq? char #\newline) 0 c) ; FIXME this will break at some point (and may be already)
                 ; note that this is NEXT location, not current location
                 (add1 position-correct)))
              (file-position input-port position-correct)
              (when (debug)
                #;
                (cumulative-offset (+ this-offset (cumulative-offset)))
                (println (list 'p1: (file-position input-port)))
                (println (list 'pnl1: (let-values ([(l c p) (port-next-location input-port)]) (list l c p))))
                (println (list 'pppppppppppppppp (peek-char input-port)))
                (println (list 'tttttttttttttttt token-correct))
                #;
                (println (list 'cumoff (cumulative-offset)))
                )
              token-correct)
            ; FIXME we will have to come up with a better solution where we can set the
            ; file position on the appended ports for the first token so that everything
            ; will compose correctly for chaining input ports

            ; NOTE we return a cons here because the lexer first-port
            ; has to handle the backtrack
            (cons token-correct position-correct)))))

(define (-stop-before-alt-branch TOKEN lexeme input-port #:eof [eof-nok #t])
  (if (and eof-nok (eq? (peek-char input-port) eof))
      ; FIXME decide how we are goin to do this and make it consistent
      (token (string->symbol (format "~a-EOF" TOKEN)) lexeme)
      (token TOKEN lexeme)))

(define (token-stop-before-table-double-blank-line TOKEN lexeme input-port start-pos)
  (if (string-suffix? lexeme "\n")
      (token TOKEN lexeme) ; this ending is correct, even if we were at eof
      (if (regexp-match #rx"\n[ \t]*\\|[^\n]*$" lexeme) ; FIXME double parse try (peek-char input-port) maybe?
          (token TOKEN lexeme) ; the last line is a well formed list
          (token-stop-before TOKEN TOKEN lexeme #\newline input-port start-pos #:eof #f))))

(define (token-stop-before-blank-line TOKEN lexeme input-port start-pos) ; FIXME confusing naming
  (if (string-suffix? lexeme "]") ; EOF case goes here I think
      (-stop-before-alt-branch TOKEN lexeme input-port)
      (let ([TOKEN-MALFORMED (string->symbol (format "~a-MALFORMED" TOKEN))])
        (token-stop-before TOKEN-MALFORMED TOKEN-MALFORMED lexeme #\newline input-port start-pos))))

(define (token-stop-before-heading TOKEN lexeme input-port start-pos #:eof [eof-nok #t])
  (if (string-suffix? lexeme "*")
      (let ([TOKEN-MALFORMED (string->symbol (format "~a-MALFORMED" TOKEN))])
        (token-stop-before TOKEN-MALFORMED TOKEN-MALFORMED lexeme #\newline input-port start-pos))
      (-stop-before-alt-branch TOKEN lexeme input-port #:eof eof-nok)))

(define (token-stop-before-foot-def TOKEN lexeme input-port start-pos)
  (if (eq? (peek-char input-port) #\])
      (token-stop-before TOKEN TOKEN lexeme #\newline input-port start-pos)
      #;
      (let ([TOKEN-MALFORMED (string->symbol (format "~a-MALFORMED" TOKEN))])
        (token-stop-before TOKEN-MALFORMED TOKEN-MALFORMED lexeme #\newline input-port start-pos))
      (-stop-before-alt-branch TOKEN lexeme input-port)))

(define (token-stop-for-paragraph TOKEN lexeme input-port start-pos)
  (if (string-suffix? lexeme "*")
      (token-stop-before-heading TOKEN lexeme input-port start-pos)
      (if (or (and (memq (peek-char input-port) '(#\+ #\space)) (string-suffix? lexeme "#"))
              ; FIXME this will hit false positives
              (and (string-suffix? lexeme ":")
                   (eq? (peek-char input-port) #\newline) ; FIXME isn't this nearly always going to be true?
                   (regexp-match #rx"\n[ \t]*:[A-Za-z0-9_-]+:[ \t]*$" lexeme) ; FIXME sigh reparse
                   ))
          (token-stop-before TOKEN TOKEN lexeme #\newline input-port start-pos)
          (if (eq? (peek-char input-port) #\])
              (token-stop-before-foot-def TOKEN lexeme input-port start-pos)
              (-stop-before-alt-branch TOKEN lexeme input-port)))))

(define (token-stop-before-heading-foot-def-double-blank-line TOKEN lexeme input-port start-pos)
  ; we don't have to do anything for the double-blank-line case
  ; because it will stop before the final newline allowing the newline
  ; first parsing to continue
  (if (string-suffix? lexeme "*")
      (token-stop-before-heading TOKEN lexeme input-port start-pos)
      (if (eq? (peek-char input-port) #\])
          (token-stop-before-foot-def TOKEN lexeme input-port start-pos)
          (-stop-before-alt-branch TOKEN lexeme input-port))))

(define (get-tokens tokenizer)
  (define (sigh next-token [accum '()])
    (let ([t (next-token)])
      (if (eq? t eof)
          accum
          (sigh next-token (cons t accum)))))
  (reverse (sigh tokenizer)))

#; ; TODO buffer local somehow? needed for efficient reparsing
(define heading-lexer (make-parameter #f))

(define (bind-runtime-todo-keywords [keywords #f])
  ; FIXME I swear this is as close to absolutely having to use
  ; eval as I have ever come, and it is because brag and agg
  ; are completely static macros with regard to specifying the tokenizer
  ; (define-syntax NAME (make-lex-abbrev (λ () (quote-syntax RE))))
  #;
  (define-syntax runtime-todo-keyword
    (make-lex-abbrev (λ () (datum->syntax #f `(:or ,@keywords)))))
  #;
  (eval-syntax #`(define-lex-abbrev runtime-todo-keyword (:or #,@keywords)))
  ;;(define-lex-abbrev eof (eof))
  (define heading-lexer-src
    ; XXX this lexer should ALWAYS be given strings that start and end with a newline
    #`(lexer-srcloc
       [(from/stop-before (:seq "\n" (:+ "*")) (:or " " "\t"))
        ; XXX stars eating the space is a design flaw, the tokenizer must peek
        (token 'STARS lexeme)]
       [(:seq (:+ (:or " " "\t"))
              (:+ (:seq ":"
                        (:* ; zero or more so that empty tags are still tags
                         (:or alpha 0-9 "_" "@" "#" "%"))))
              ; XXX warning :+ seems to have an implicit :or lurking in it
              ; XXX unicode dialect and ascii dialect?
              ":"
              ; FIXME SIGH we can't include eof here because ... reasons ??
              (:* (:or " " "\t"))
              "\n")
        (token 'TAGS lexeme)]
       ["COMMENT" (token 'CHARS-COMMENT lexeme)] ; this must come befor RTK
       #; ; we don't need this anymore it complicates the grammar signiciantly
       ; it was originally retained to deal with divergent behavior between org-element and the archive
       ; functionality itself, but since we have fixed the issue and are using stop-before, we don't need
       ; this anymore
       [":ARCHIVE:" (token 'ARCHIVE lexeme)] ; we keep this so we can catch lone archive tags more easily
       #;
       [runtime-todo-keyword (token 'RUNTIME-TODO-KEYWORD lexeme)]
       [(:or #,@keywords) (token 'RUNTIME-TODO-KEYWORD lexeme)]
       [(:seq "[#" upper-case "]") (token 'PRIORITY lexeme)]
       #;
       [":" (token 'COLON lexeme)]
       #;
       ["[" (token 'LSB lexeme)]
       #;
       ["]" (token 'RSB lexeme)]
       #;
       ["#" (token 'HASH lexeme)]
       ; FIXME markup is allowed here too
       [(:or " " "\t") (token 'BLANK lexeme)] ; unfortunately we still have to split on space
       [(:seq (:+ (:~ #;"*" "[" "]" ":" "\n" " " "\t")))
        (token 'OTHER lexeme)]
       ["\n" (token 'NEWLINE-END)]
       [(:~ (:or " " "\t" "\n" #;":")) ; insurance
        #;
        any-char
        #;(:~ "*")
        (token 'OOPS lexeme)]))
  (define heading-lexer (eval-syntax heading-lexer-src (namespace-anchor->namespace nsa)))
  ; FIXME apparently this is incredibly slow, taking up nearly a quarter of our runtime !?
  (define (heading-make-tokenizer port)
    ; FIXME can't run compile-syntax due to use of eval above probably?
    (define (next-token)
      (let ([out (heading-lexer port)])
        (when (debug)
          (displayln 'heading-tok:)
          (pretty-print out))
        out))
    next-token)
  heading-make-tokenizer)

(module+ test-heading
  (require laundry/heading)
  (define heading-make-tokenizer (bind-runtime-todo-keywords '("TODO" "DONE" "CERT")))
  (get-tokens (heading-make-tokenizer (open-input-string "\n* h :t:\n")))
  (get-tokens (heading-make-tokenizer (open-input-string "\n* TODO h :t:\n")))
  (get-tokens (heading-make-tokenizer (open-input-string "\n* CERT [#A] h :t:\n")))
  (get-tokens (heading-make-tokenizer (open-input-string "\n** COMMENT DONE [#B] z : z :t:\n")))
  (define (hrm str) (heading-make-tokenizer (open-input-string str)))
  (get-tokens (hrm "\n** COMMENT DONE [#B] z aaaaaaaaaaaa aaaaaaaaaaaaaa : aaaaaaaaaaa z :t:\n"))
  (get-tokens (heading-make-tokenizer (open-input-string "\n** COMMENT DONE [#B] z [lol] : z :t:\n")))
  (get-tokens (heading-make-tokenizer (open-input-string "\n* :t:ARCHIVE:\n")))
  (parse-to-datum (hrm "\n** COMMENT DONE [#B] z aaaaaaaaaaaa aaaaaaaaaaaaaa : aaaaaaaaaaa z :t:\n"))
  (parse-to-datum (hrm "\n* TODO something\n"))
  (parse-to-datum (hrm "\n*** LOL ***\n"))

  )

; FIXME TODO consider using parameters to avoid emitting duplicate
; markup but consider also that this may not be the best or even
; correct place to attempt such a thing

(define paragraph-lexer
  (lexer-srcloc
   [macro-invocation (token 'MACRO lexeme)]
   #; ; the spec for macro reads like it can be multi-line
   [(:seq "{{{" alpha (:+ (:or alpha 0-9 "-" "_")) (:? (:seq "(" ")")) "}}}") (token 'MACRO lexeme)]
   [(:+ (:~ mu-pre-1 mu-marker mu-post-1 "]")) (token 'STUFF-B lexeme)]

   #|
   [markup-*/_+ (token 'MU-BIUS lexeme)]

   [markup-*/+ (token 'MU-BIS lexeme)]
   [markup-*/_ (token 'MU-BIU lexeme)]
   [markup-*_+ (token 'MU-BUS lexeme)]
   [markup-/_+ (token 'MU-IUS lexeme)]

   [markup-*/ (token 'MU-BI lexeme)]
   [markup-*_ (token 'MU-BU lexeme)]
   [markup-*+ (token 'MU-BS lexeme)]
   [markup-/_ (token 'MU-IU lexeme)]
   [markup-/+ (token 'MU-IS lexeme)]
   [markup-_+ (token 'MU-US lexeme)]
   |#

   [markup-* (token-back-1 'BOLD lexeme input-port)]
   [markup-/ (token-back-1 'ITALIC lexeme input-port)]
   [markup-_ (token-back-1 'UNDERLINE lexeme input-port)]
   [markup-+ (token-back-1 'STRIKE lexeme input-port)]

   [markup-= (token-back-1 'VERBATIM lexeme input-port)]
   [markup-~ (token-back-1 'CODE lexeme input-port)]

   #; ; busted
   [(:+ (:or mu-pre mu-post)) (token 'STUFF-A lexeme)]

   [(:+ (:or (:& (:~ mu-pre-1) mu-post-not-newline) mu-marker)) (token 'STUFF-A lexeme)]

   [mu-pre-n-not-lcb (token 'MU-PRE-N-NOT-LCB lexeme)] ; LCB cannot be in mu-pre-n because it would block macro
   [mu-pre-1 (token 'MU-PRE-1 lexeme)] ; needed to prevent accidental capture when :+ length for stuff is longer than markup

   [citation (token 'CITATION lexeme)] ; FIXME annoying because it gets parsed twice

   [footnote-anchor (token 'FOOTNOTE-ANCHOR lexeme)]
   #; ; doesn't work in the absense of EOF might work if we detect eof in here
   [footnote-inline-malformed
    ; I think this kind of works? we might not back up enough? we hard match \n\n without stop before
    (begin
      (println (list "BEFORE" lexeme))
      (file-position input-port (sub1 (file-position input-port))) ; FIXME port-next-location
      (let-values ([(l c p) (port-next-location input-port)])
        (set-port-next-location! input-port l 0 (sub1 p)))
      (println "I THOUGH SO")
      (begin0
          (token-stop-before-blank-line 'FOOTNOTE-INLINE-MALFORMED (substring lexeme 0 (sub1 (string-length lexeme))) input-port start-pos)
        (println "I don't get here"))
      )]
   [footnote-inline-start (token 'FOOTNOTE-START-INLINE lexeme)]
   #; ; XXX leave this out for now so we can test a uniform solution
   [footnote-inline-simple (token 'FOOTNOTE-INLINE-SIMPLE lexeme)]

   #; ; busted
   [maybe-paired-square ; FIXME risk of matching whole footnotes?
    (if (regexp-match #rx"\\[" (substring lexeme 1))
        (token 'NOT-INLINE-FOOTNOTE lexeme) ; FIXME obvious failure with [fn::[fn::]] case
        (token 'PAIRED-SQUARE lexeme))
    ]

   [hyperlink (token 'LINK lexeme)]
   [hyperlink-ab (token 'LINK-AB lexeme)]
   [timestamp (token 'TIMESTAMP lexeme)]

   ["[" (token 'LSB lexeme)]
   ["]" (token 'RSB lexeme)]

   #;
   [(:+ mu-marker) (token 'MARKER lexeme)] ; break out from the pre/post to avoid creating longer matches than eof? cases

   #;
   [(:+ (:or mu-pre-not-newline mu-post-not-newline)) (token 'STUFF-A lexeme)]
   ["\n" (token 'NEWLINE)]
   )
  #;
  (
   [(:>= 2 whitespace) (token 'WHITESPACE-GT2 lexeme)]


   ; not clear this is needed
   [" " (token 'SPACE)]
   ["\t" (token 'TAB)]
   ["-" (token 'HYPHEN lexeme)]
   ["." (token 'PERIOD lexeme)]
   ["," (token 'COMMA lexeme)]
   [";" (token 'SC lexeme)]
   [":" (token 'COLON lexeme)]
   ["'" (token 'SQ lexeme)]
   ["\"" (token 'DQ lexeme)]
   [")" (token 'R-PAREN lexeme)]
   ["}" (token 'RCB lexeme)]
   ["[" (token 'LSB lexeme)]
   ["!" (token 'BANG lexeme)]
   ["?" (token 'QM lexeme)]

   ))

(module+ test-paragraph
  ; FIXME terminal double newline never makes it through the top level lexer
  (paragraph-lexer (open-input-string "[fn::a[fn::b]]"))

  (paragraph-lexer (open-input-string "[fn:: sigh\n\nmore"))

  (let ([port (open-input-string "\n[fn:: sigh\n\nmore")])
    (list
     (paragraph-lexer port)
     (paragraph-lexer port)
     (paragraph-lexer port)
     ))

  (let* ([port (open-input-string "[fn::sigh\nmore")] ; here's our problem, \n\n never shows up
         [next-token (paragraph-make-tokenizer port)])
    (list (next-token) (next-token)))

  )

(define table-lexer
  (lexer-srcloc
   [(:seq "\n" (:* (:or "\t" " ")) "|" "-" (:* (:~ "\n")))
    (token 'TABLE-ROW-RULE lexeme)]
   [(:seq "\\" "|") (token 'ESC-PIPE "|")]
   ["|" (token 'PIPE lexeme)]
   #;
   ["-" (token 'HYPHEN lexeme)]
   ["\n" (token 'NEWLINE)]
   ["\t" (token 'TAB)]
   [" " (token 'SPACE)]
   [(:+ (:~ (:or "|" #;"-" "\\" "\t" " " "\n"))) (token 'REST lexeme)]))

(module+ test-table
  ; table element stop before is not straight forward
  (define test-table-lexer
    (lexer-srcloc
     [table-element
      #;(token 'TABLE-ELEMENT lexeme)
      (token-stop-before-table-double-blank-line 'TABLE-ELEMENT lexeme input-port start-pos)]))

  ; manual newline because we aren't using all the bof/eof fixes
  (test-table-lexer (open-input-string "\n |\n x\n"))
  (test-table-lexer (open-input-string "\n|\nx|"))
  (test-table-lexer (open-input-string "\n|\nx"))
  (test-table-lexer (open-input-string "\n|\n"))
  (test-table-lexer (open-input-string "\n|"))

  )

(define (paragraph-make-tokenizer port)
  (define (next-token)
    (let ([out (paragraph-lexer port)])
      (when (debug)
        (println ':paragraph-make-tokenizer)
        (pretty-print out))
      out))
  next-token)

(define (table-make-tokenizer port)
  (define (next-token)
    (let ([out (table-lexer port)])
      #;
      (begin
        (println ':table-make-tokenizer)
        (pretty-print out))
      out))
  next-token)

; I have no idea why this was defined inside of make-tokenizer in the
; original example I copied years ago ...

; ah, now I see, sometimes you want/need to be able to configure the
; lexer at runtime, so for exaple org reconfigures startup options
; searching out the initial configuration of various #+todo: keywords
; which may appear anywhere in the file (oops) so a full simple parse
; would have to be conducted to find them first, OR we specify that
; #+todo: keywords defined outside the first section of the org-file
; are not guranteed to be included during any given parse

; XXX FALSE, there is no easy way to define lexer-srcloc
; why the foo do people define these things as macros ?!?!?!?!
; there is no reason to force it to be so fooing rigid
; all in the name of safety I'm sure, so annoying though
; why the hell can't I just pass the data in!

; XXX further fooery: ah yes, even more problems which is
; that you can't compile the fooing lexer so that it is
; performant at runtime when you need it because this thing
; wait no ... this can't be the case ... because the macro
; used to be invoked only inside a function call ?
; ah foo, but it could still be compiled ahead of time

(define-namespace-anchor nsa)

(define laundry-lexer
  (lexer-srcloc

   ; for testing secondary headline parser
   #;
   ["TODO" (token 'RUNTIME-TODO-KEYWORD lexeme)]
   #;
   ["DONE" (token 'RUNTIME-TODO-KEYWORD lexeme)]

   [heading (begin0 (token 'HEADING lexeme)
              #;
              (println (list
                        "heading start-pos" start-pos
                        "file-position input-port after heading" (file-position input-port)
                        input-port
                        (port-file-identity input-port)
                             )))
            ]

   ; FIXME there is a question of whether to use the lexer like this
   ; to slurp code blocks in the first pass and then run code block
   ; specific parser in a later pass, or whether we want to try to
   ; get certain information in the first pass
   #; ; XXX doesn't work the match is too short
   [(from/stop-before (:seq "\n" (:* " " "\t") "#+begin_src")
                      (:seq "\n" (:+ "*") (:or " " "\t")))
    (token 'SRC-BLOCK-BEGIN-MALFORMED lexeme)]
   ; XXX we can't do start-after on a heading to find a random end_src
   #; ; easier to dispatch all blocks from the same patterns and issue tokesn based on block-type
   [src-block (token-stop-before-heading 'SRC-BLOCK lexeme input-port start-pos)]
   [(from/stop-before unknown-block-line-end "\n") ; if we run into one of these by itself it is malformed because it is disconnect
    (token 'UNKNOWN-BLOCK-MALFORMED lexeme)]
   [(from/stop-before unknown-block-line-begin "\n")
    (let* ([block-type (get-block-type lexeme)]
           [token-name
            (case block-type
              ; the only blocks that are differentiated here are types
              ; that are differentiated here because they are the only
              ; ones that have parsing impliciations at runtime

              ; note that without smuggling in some state to this
              ; lexer we can't really lookahead and then parse a
              ; separate language in here
              [("src" "SRC") 'SRC-BLOCK]
              #; ; TODO example, verbatim, etc. that need special fontification
              [("example" "EXAMPLE") 'EXAMPLE-BLOCK]
              [else 'UNKNOWN-BLOCK])]
           [make-lexer
            (λ ()
              (let ([stx
                     #`(lexer
                        [(from/stop-before
                          "" ; optional to handle the case where the end line is immediate
                          (:or stop-before-heading
                               (:& (:seq "\n"
                                         (:* " " "\t") (:or "#+end_" "#+END_") #,block-type (:* " " "\t")
                                         "\n")
                                   (:seq any-string (:or "#+end_" "#+END_") #,block-type any-string))))
                         ; FIXME may need a few more values out of this
                         lexeme])])
                #;
                (log-error "make-lexer stx: ~a" stx)
                (eval-syntax stx (namespace-anchor->namespace nsa))))]
           [block-lexer (make-lexer)]
           [lexeme-more (block-lexer input-port)]
           [lexeme-combined (string-append lexeme lexeme-more)]
           [was-heading (regexp-match #rx"\n[*]+$" lexeme-more)]
           )
      (when (debug)
        (println (list "eeeeeeee:" block-type lexeme lexeme-more lexeme-combined)))
      (if was-heading
          (let* ([offset (string-length (car was-heading))]
                 [tok (token 'UNKNOWN-BLOCK-MALFORMED
                             (substring lexeme-combined 0 (- (string-length lexeme-combined) offset)))]
                 [comb (λ (in)
                         (file-position in (- (file-position in) offset))
                         (set-port-position! in offset #:back-line #t))])
            #;
            (log-error "was-heading: ~a" offset)
            ; FIXME backtrack at bof and eof has to be done outside this function TODO put this in a function
            (if (or (file-stream-port? input-port)
                 (string-port? input-port))
                (begin
                  (comb input-port)
                  tok)
                ; XXX resolved down in -stt below FIXME naming and action at a distance
                (cons tok comb))
            #;
            (error was-heading)
            ; XXX note that if we hit malformed all the rest of the
            ; text to the next headline is marked as malformed, the
            ; elisp implementation has different behavior
            #;
            (token 'UNKNOWN-BLOCK-MALFORMED (substring lexeme-combined 0 (- (string-length lexeme-combined) offset))))
          (if (string=? (string-append lexeme "\n") lexeme-combined)
              ; FIXME this is the result of stop/before matching
              (token 'UNKNOWN-BLOCK-MALFORMED lexeme-combined) ; eof case or immediate heading case
              (begin
                #;
                (println (list 'aaaaaaaaaaaaaaaaaaa lexeme lexeme-combined))
                (token token-name lexeme-combined)))))]
   #;
   [unknown-block ; FIXME somehow this is taking priority over src-block? or what?
    ; TODO we should be able to rework this by detecting #+begin_pattern, extracting pattern
    ; and then doing (from/stop-before "\n" "#+end_pattern") and synthesizing the lexeme
    ; it is not clear that this approach is the best since it means that a different approach
    ; will likely have to be taken if you are using a less powerful tokenizer that can't
    ; to aribtrary things ... actually, this matches very will with what tree sitter provides
    ; in terms of external scanners, their examples include heredocs etc. which are a classic
    ; example of context sensitivity
    (let* (#;[test-lexeme (string-downcase lexeme)] ; FIXME case folding is evil, we should not support this
           [suffix (string-trim (car (regexp-match "_[^ \t]+[ \t]" lexeme)) #:repeat? #t)]
           [sigh (last (string-split (string-trim lexeme "\n" #:repeat? #t) "\n"))]
           )
      (unless (or (regexp-match #rx"^[*]+$" sigh) #;(regexp-match #rx"\n[*]+$" lexeme)
                  (regexp-match (regexp suffix)
                                ; FIXME super inefficient check!
                                sigh)
                  (regexp-match #rx"\n\\*+[ \t]$" lexeme))
        ; TODO proper next steps when a mismatch is detected
        ; FIXME elisp can deal with nested cases ... at least for example blocks
        (error "mismatch" sigh suffix start-pos))
      (if (member suffix '("_src" "_SRC"))
          (token-stop-before-heading 'SRC-BLOCK lexeme input-port start-pos #:eof #f) ; FIXME workaround ...
          (token-stop-before-heading 'UNKNOWN-BLOCK lexeme input-port start-pos #:eof #f)))]

   [table-element
    ; FIXME this still has issues where the newline following the last | is dropped
    (token-stop-before-table-double-blank-line 'TABLE-ELEMENT lexeme input-port start-pos)]
   [keyword-element (token 'KEYWORD-ELEMENT lexeme)] ; before hyperlink for #+[[[]]]:asdf
   #;
   [hyperlink (token 'LINK lexeme)] ; as it turns out this also helps performance immensely
   #;
   [hyperlink-ab (token 'LINK-AB lexeme)]
   ; in theory it should be possible to scan for headlines and then parse all the sections
   ; in parallel

   [comment-element (token 'COMMENT-ELEMENT lexeme)]

   [drawer-props (token-stop-before-heading 'DRAWER-PROPS lexeme input-port start-pos)]
   [drawer-ish
    (begin0
        (token-stop-before-heading 'DRAWER lexeme input-port start-pos)
      #;
      (println (list
                "drawer start-pos" start-pos
                "file-position input-port after drawer" (file-position input-port)
                input-port
                (port-file-identity input-port)
                ))
      )]

   [paragraph ; FIXME don't return PARAGRAPH-MALFORMED here
    (token-stop-for-paragraph 'PARAGRAPH lexeme input-port start-pos)
    ] ; needed for performance reasons to mitigate quadratic behavior around short tokens
   [paragraph-2 (token 'PARAGRAPH-2 lexeme)] ; the only time this will match is if paragraph does not so we ware safe

   [(:>= 2 "*") (token 'STARS lexeme)] ; need this in lexer otherwise performance tanks in the parser
   ["*" (token 'ASTERISK lexeme)]
   ["\n" (token 'NEWLINE)]
   [" " (token 'SPACE)]
   #; ; TODO should help with perf
   [(:+ " ") (token 'SPACE-N lexeme)]
   ["\t" (token 'TAB)]

   #; ; don't parse this at the top level, only in the paragraph
   [citation (token 'CITATION lexeme)] ; TODO will likely want nested tokenization here

   ; these are lexemes so that we can ignore them if they don't start the line

   #;
   ["[fn:" (token 'FOOTNOTE-START lexeme)]
   #; ; eaten by paragraph in nearly all cases I think
   [footnote-anchor (token 'FOOTNOTE-ANCHOR lexeme)]
   #; ; eaten by paragraph in nearly all cases I think
   [footnote-inline-start (token 'FOOTNOTE-START-INLINE lexeme)]
   [footnote-definition
    (token-stop-before-heading-foot-def-double-blank-line
     'FOOTNOTE-DEFINITION lexeme input-port start-pos)]
   #;
   ["[fn::" (token 'FOOTNOTE-START-INLINE lexeme)] ; here longest match helps us

   ["COMMENT" (token 'CHARS-COMMENT lexeme)] ; FIXME ALPHA-N takes priority over this >_< FFS

   ["DEADLINE" (token 'CHARS-DEADLINE lexeme)]
   ["SCHEDULED" (token 'CHARS-SCHEDULED lexeme)]
   ["OPENED" (token 'CHARS-OPENED lexeme)]
   ["CLOSED" (token 'CHARS-CLOSED lexeme)]

   ; FIXME fontification doesn't work for lower case but org element does?
   ; no, org element recognizes that it might be a planning line but cannot parse the lower case
   ; seek clarity
   ;[(:or "deadline:" "DEADLINE:") (token 'DEADLINE lexeme)]
   ;[(:or "scheduled:" "SCHEDULED:") (token 'SCHEDULED lexeme)]
   ;[(:or "closed:" "CLOSED:") (token 'CLOSED lexeme)]

   #; ; I don't think we need this anymore
   [":ARCHIVE" (token 'ARCHIVE lexeme)] ; sigh longest match >_<

   ;[":no_export" (token 'NO_EXPORT lexeme)] ; TODO this should probably be configurable

   ; FIXME why is this so uselessly static ... SIGH
   ;[runtime-todo-keyword (token 'CHARS-TODO-KEYWORD lexeme)]
   ;[hrm (token 'CHARS-TODO-KEYWORD lexeme)]

   #;
   [(:or ":PROPERTIES:" ":properties:") (token 'PROPERTIES-D lexeme)] ; sigh
   #;
   [(:or ":END:" ":end:") (token 'END-D lexeme)] ; sigh

   ; FIXME yeah, this is is a pain
   #;
   [(:or "#+begin_src" "#+BEGIN_SRC") (token 'BEGIN-SRC lexeme)]
   #;
   [(:or "#+end_src" "#+END_SRC") (token 'END-SRC lexeme)]
   #;
   [(:or "#+begin_example" "#+BEGIN_EXAMPLE") (token 'BEGIN-EX lexeme)]
   #;
   [(:or "#+end_example" "#+END_EXAMPLE") (token 'END-EX lexeme)]

   [(from/stop-before (:or "#+begin_" "#+BEGIN_") whitespace)
    (token 'UNKNOWN-BLOCK-MALFORMED lexeme)]
   [(from/stop-before (:or "#+end_" "#+END_") whitespace)
    (token 'UNKNOWN-BLOCK-MALFORMED lexeme)]

   [(:or "#+begin" "#+BEGIN") (token 'BEGIN-DB lexeme)]
   [(:or "#+end" "#+END") (token 'END-DB lexeme)]

   #; ; this does not work as desired
   [
    ; XXX fortunately this does not capture #+begin_src style lines due to sligh differences in syntax
    ; this captures the essence of the keyword line
    ; dealing with the ambiguity of the current specification is
    ; more problematic
    ; FIXME this won't work because whitespace IS allowed inside [] so we can't defer
    ; resolving cases like #+k[[[]:v ]]
    (:or
     (from/stop-before (:seq "\n"
                             (:+ " " "\t")
                             "#+"
                             (:+ (:~ whitespace)) ; pretty sure this gobbles the []
                             ; this is ambiguous and it is not obvious what the rigution
                             "["
                             (:~ "]" "\n") ; FIXME nesting issues
                             "]:"
                             (:* (:~ ":" "\n")))
                       "\n")

     (from/stop-before (:seq "\n"
                             (:+ " " "\t")
                             "#+"
                             (:+ (:~ whitespace))
                             ":"
                             (:* (:~ ":" "\n")))
                       "\n"))
    (token 'KEYWORD-LINE lexeme)]
   ; FIXME 99% these should not be in the tokenizer maybe with #+NAME: #+name:
   ; the issue is that if these appear at other places in the file we will be
   ; in trouble ; NOTE have to add the colons ourselves where needed
   ; XXX eh, we've more or less gotten it all worked out here
   ; affiliated keywords
   #|
   [(:or "#+NAME" "#+name") (token 'NAME lexeme)]
   [(:or "#+HEADER" "#+header") (token 'HEADER lexeme)]
   [(:or "#+PLOT" "#+plot") (token 'PLOT lexeme)]
   [(:or "#+RESULTS" "#+results") (token 'RESULTS lexeme)] ; NOTE THE PLURAL
   [(:or "#+CAPTION" "#+caption") (token 'CAPTION lexeme)]
   [(:or "#+ATTR_" "#+attr_") (token 'ATTR-PREFIX lexeme)]
   |#

   #; ; we can handle this more cleanly in the keyword module of the expander
   [todo-spec-line (token 'TODO-SPEC-LINE lexeme)]

   #;
   [(:or "#+TODO" "#+todo") (token 'TODO lexeme)]
   #; ; these arent' actually affilated so removing them to simplify things
   [(:or "#+AUTHOR" "#+author") (token 'AUTHOR lexeme)]
   #;
   [(:or "#+DATE" "#+date") (token 'DATE lexeme)]
   #;
   [(:or "#+TITLE" "#+title") (token 'TITLE lexeme)] ; FIXME this is showing up as an affiliated keyword
   
   ; the call ... not actually keyword and not actually associated
   ; XXX TODO but should be
   [(:or "#+CALL" "#+call") (token 'CALL lexeme)] ; FIXME move to the keyword module of the expander

   ;[(from/to "@@" "@@" (token 'EXPORT-SNIPPET lexeme))] ;; TODO this might actually work ... except for nested blocks
   ;[month-major-digit (token 'MMD)]
   ;[day-major-digit (token 'DMD)]
   ;[time-major-digit (token 'TMD)]
   ;[(eof) (token 'MY-EOF) #;(return-without-srcloc eof)]
   [(eof) (return-without-srcloc eof)]
   ; [(eof) (token-EOF)]
   ;["#+" (token 'HASH-PLUS)] ; do not want
   ;[(from/to "#" "\n") (token 'COMMENT lexeme)] ; lol can't use this
   ;[whitespace (token 'WS lexeme)]

   [":" (token 'COLON lexeme)] ; XXX not sure what needs this but it is related to drawer tests
   ["_" (token 'UNDERSCORE lexeme)] ; XXX something in test keywords needs this

   ; the keyword failures FIXME keyword failures should probably parse as that directly?
   ["+" (token 'PLUS lexeme)] ; #+ needs this apparently? ; FIXME
   ["[" (token 'LSB lexeme)]  ; #+x[ ]x: ; FIXME
   ["]" (token 'RSB lexeme)]  ; #+x[ ]x: ; FIXME
   ["." (token 'PERIOD lexeme)] ; needed for ordered lists  ; FIXME lex those lines
   [")" (token 'R-PAREN lexeme)] ; needed for ordered lists  ; FIXME lex those lines
   ["-" (token 'HYPHEN lexeme)] ; needed for unordred lists ?? ; FIXME
   ["@" (token 'AT lexeme)] ; needed for the list start [@99]

   #| ; I'm pretty sure we can drop all of this now

   ["#" (token 'HASH lexeme)]
   ["%" (token 'PERCENT lexeme)]

   ["|" (token 'PIPE lexeme)]
   ["<" (token 'LAB lexeme)]
   [">" (token 'RAB lexeme)]
   ["{" (token 'LCB lexeme)]
   ["}" (token 'RCB lexeme)]
   ["'" (token 'SQ lexeme)]
   ["\"" (token 'DQ lexeme)]
   ["\\" (token 'BS lexeme)]

   ["(" (token 'L-PAREN lexeme)]
   |#
   
   ; strings don't work because longest match means we can never detect the escape sequence
   ;[(from/to "\"" "\\") (token 'DQ-TO-ESC lexeme)]
   ;[(from/to "\"" "\"") (token 'DQ-TO-DQ lexeme)] ; ARGH longest match kills us here again :/

   [0-9 (token 'DIGIT lexeme)]
   #|
   [(:= 2 0-9) (token 'DIGIT-2 lexeme)] ; dates
   [(:= 3 0-9) (token 'DIGIT-3 lexeme)] ; dates
   [(:= 4 0-9) (token 'DIGIT-4 lexeme)] ; dates
   |#
   [(:>= 2 0-9) (token 'DIGIT-N lexeme)]
   ;["l" (token 'CHAR-LOWER-L lexeme)] ; HAH BEGONE FOUL INSTANCE OF A THING
   ["X" (token 'CHAR-UPPER-X lexeme)] ; FOO
   [alpha (token 'ALPHA lexeme)] ; FIXME TODO alpha+ ?
   [(:>= 2 alpha) (token 'ALPHA-N lexeme)]
   #;
   [(:>= 2 lower-case) (token 'ALPHA-LOWER-N lexeme)]
   #; ; given the rework of the parsing hierarchy we don't need these to be top level tokens
   [(:** 2 7 upper-case) (token 'ALPHA-UPPER-N lexeme)] ; XXX stop at 7 to avoid gobbling COMMENT and ARCHIVE
   ; TODO with ARCHIVE it might be possible to match against :ARCHIVE
   ; for COMMENT it is trickier ~* COMMENT~ ~]COMMENT~ but then we're back to where we started
   ; so suggestion to come up with some prefix for comment or modify the grammar have COMMENT
   ; come BEFORE the todo keyword if it is present at all, this will allow us to increase the run
   ; length of the ALPHA-N parse, the alternative is to ditch matching COMMENT during the first
   ; parse pass, but that seems like it might be a bad tradeoff, can also split alpha into
   ; upper and lower allow lower to match N since lower case are used more frequently

   ; FIXME l breaks this (SIGH switches) TODO I think the right way to
   ; handle this is to have switch syntax just be switch-sign ALPHA
   ; string | switch-sign ALPHA but then the last string gets gobbled,
   ; so have to split the rules for the params if there are switches

   ;[word (token 'WORD-CHAR lexeme)] ; FIXME need punctuation here

   [(:+ negated-set) (token 'NEGATED-SET lexeme)] ; massively enhance parsing performance via fallthrough XXX TODO 
   )
  ; this is probably going to break the parser until
  ; we adjust the fact that NETAGED-SET matches more than 1 char now?
  ; but actually looking at this, I don't think there are any cases where this is used
  ; that will break as a result because it will just keep going until it hits a negated
  ; token at which point the logic for the next step will kick in which should be almost
  ; still need to check though
  )

#; ; default to #f to force a contract violation if not parameterized correctly
(define cumulative-offset (make-parameter #f))

(define (fix-srcloc-mod srcloc-token-instance)
  (let* ([token (srcloc-token-token srcloc-token-instance)]
         [value (token-struct-val token)]
         [actual-span (if (string? value) (string-length value) 1)]
         [srcloc-raw (srcloc-token-srcloc srcloc-token-instance)]
         [span (srcloc-span srcloc-raw)]
         [srcloc (make-srcloc
                  (srcloc-source srcloc-raw)
                  (srcloc-line srcloc-raw)
                  (srcloc-column srcloc-raw)
                  (srcloc-position srcloc-raw)
                  #;
                  (if #f
                      ; FIXME position gives correct numbers now when
                      ; parsing from a file but the actual file position
                      ; is still broken and this also breaks the
                      ; so it looks like somehow when using string ports
                      ; resetting file-position works correctly but not for file ports !?
                      (- (srcloc-position srcloc-raw) (cumulative-offset))
                      (srcloc-position srcloc-raw))
                  (if (not (= actual-span span))
                      actual-span
                      #;
                      (begin
                        (cumulative-offset (+ (cumulative-offset) (- span actual-span)))
                        actual-span)
                      span))]
         [srcloc-token-out (make-srcloc-token token srcloc)])
    (when (debug)
      (println (list "fix-srcloc-mod out:" srcloc-token-out)))
    srcloc-token-out))

(define (fix-srcloc-eof srcloc-token-instance actual-start end-consumed)
  (let* ([stt (srcloc-token-token srcloc-token-instance)]
         [value (token-struct-val stt)]
         [nl-end? (and (string? value) (string-suffix? value "\n"))]
         [token (token
                 (token-struct-type stt)
                 (if nl-end?
                     (substring value 0 (- (string-length value) end-consumed))
                     value))]
         [sl (srcloc-token-srcloc srcloc-token-instance)]
         [srcloc (make-srcloc
                  (srcloc-source sl)
                  (srcloc-line sl)
                  (srcloc-column sl)
                  actual-start
                  (if nl-end?
                      (sub1 (srcloc-span sl))
                      (srcloc-span sl)))]
         [srcloc-token-out (make-srcloc-token token srcloc)])
    (when (debug)
      (println (list "fix-srcloc-eof out:" srcloc-token-out)))
    srcloc-token-out))

(define (fix-srcloc-bof -first-out port)
  (let* ([shift 1]
         [first-token (srcloc-token-token -first-out)]
         [shifty (cons? first-token)]
         [first-out (if shifty
                        (begin
                          #;
                          (println 'SHIFTY)
                          ; FIXME this is a hack, we need to have a variant of
                          ; input-port-append that seeks the second stream
                          (file-position port (- (cdr first-token) shift))
                          ;(set-port-position! port shift) ; XXX not clear we need this
                          (car first-token))
                        first-token)]
         [out-raw
          (make-srcloc-token
           (let* ([t first-out]
                  #;
                  [__ (println (cons 'qq t))]
                  [v (token-struct-val t)]
                  [nl-start? (and (string? v)
                                  (eq? (string-ref v 0) #\newline))]
                  )
             ; FIXME must strip the leading newline
             #;
             (println (cons 'vvvvvvvvvvvvvvvvvvvvvvvvvv v))
             (token
              (token-struct-type t)
              (if nl-start?
                  ; TODO this is the best we can do for stripping out the first newline
                  ; until we can confirm that the grammar doesn't need newline at bof
                  ; however I suspect we will confirm that there will always be cases
                  ; where we need newline at bof
                  (substring v 1)
                  v)))
           ; FIXME totally screwed up on the shift for malformed drawers
           ; possibly because the port position reset has not translated
           ; or is interacting with how we calcualte shift
           (let* ([sl ; FIXME surely the location will be incorrect if things shifted
                   (srcloc-token-srcloc -first-out)]
                  [line (srcloc-line sl)]
                  [srcloc-correct
                   (make-srcloc
                    (srcloc-source sl)
                    (if line (sub1 line) line) ; FIXME careful with -> 0
                    (srcloc-column sl)
                    (srcloc-position sl) ; should always be 1
                    ;;(sub1 (srcloc-span sl))
                    ;#; ; FIXME something is broken here for the shifty case
                    (- (srcloc-span sl) shift))])
             (when (debug)
               (println (cons 'eeeeeeeeeeeeeeeeeee srcloc-correct)))
             srcloc-correct
             ))])
    #;
    (println (cons 'fpfpfppfpfp (file-position first-port)))
    #;
    (pretty-print out-raw)
    out-raw))

(define (fix-srcloc-bof-eof srcloc-token-instance port end-consumed)
  (let* ([-stt (srcloc-token-token srcloc-token-instance)]
         [shifty (cons? -stt)]
         [stt (if shifty
                  (begin
                    (if (procedure? (cdr -stt)) ; this is where we resolve the offset combinator
                        ((cdr -stt) port)
                        ; FIXME pretty sure this is broken
                        (file-position port (sub1 (cdr -stt))))
                    (car -stt))
                  -stt)]
         [value (token-struct-val stt)]
         [nl-start? (and (string? value) (eq? (string-ref value 0) #\newline))]
         [nl-end? (and (string? value) (string-suffix? value "\n"))]
         [token (token
                 (token-struct-type stt)
                 (cond [(and nl-start? nl-end?) (substring value 1 (- (string-length value) end-consumed))]
                       [nl-start? (substring value 1)]
                       [nl-end? (error "token: nl-end? should not happen ~a" stt)]
                       [(eq? (token-struct-type stt) 'NEWLINE) value]
                       [else (error "token: else should not happen ~a" stt)]))]
         [sl (srcloc-token-srcloc srcloc-token-instance)]
         [line (srcloc-line sl)]
         [srcloc (make-srcloc
                  (srcloc-source sl)
                  (if line (sub1 line) line) ; FIXME careful with -> 0
                  (srcloc-column sl) ; FIXME why weren't we modifying this?
                  1 ; this should always be one but TODO we should check
                  (cond [(and nl-start? nl-end?) (- (srcloc-span sl) (add1 end-consumed))]
                        [nl-start? (sub1 (srcloc-span sl))]
                        [nl-end? (error "srcloc: nl-end? should not happen ~a" stt)]
                        [(eq? (token-struct-type stt) 'NEWLINE) (srcloc-span sl)]
                        [else (error "srcloc: else should not happen ~a" stt)]))])
    (make-srcloc-token token srcloc)))

(define (process-first port)
  (let* ([port-end (open-input-string "\n")] ; FIXME there are forms that end in a double blank line: footnotes.
         [first-port
          (input-port-append
           #f
           (open-input-string "\n")
           port ; trailing newline gives bof-eof case for free
           port-end)])
    ; this works because the position of the original port is
    ; correctly shifted so its coordinates should not be disturbed
    (let* ([-first-out (laundry-lexer first-port)]
           [end-consumed (file-position port-end)]
           [last-out? (> (file-position first-port) (+ (file-position port) end-consumed))]
           #;
           [__ (println (list 'wat -first-out end-consumed))]
           [out (if last-out?
                    (fix-srcloc-bof-eof -first-out port end-consumed)
                    (fix-srcloc-bof -first-out port))])
      #;
      (pretty-print out)
      out)))

(define final-port (make-parameter #f)) ; use for any tokenizer that needs a final-port

(define (internal-rest port out-raw)
  (if (and (not (eq? out-raw eof)) ; not eof and not newline AND eof is next
           (not (let ([t (srcloc-token-token out-raw)])
                  #;
                  (println (cons 'asdf t))
                  (or (eq? (token-struct-type t) 'NEWLINE)
                      (let ([v (token-struct-val t)]) ; could be SPACE etc.
                        (and v (string-suffix? v "\n"))))))
           ; TODO might be able to add checks to limit cases where we need to reparse
           (eq? (peek-char port) eof)) ; TODO and lexeme end != newline
      out-raw
      #; ; FIXME this is so badly broken that sometimes you wind up with a token that ends before it starts
      (begin
        (println "internal-rest should have hit eof")
        ; FIXME we know the length of these files so there is no reason to peek?! these
        ; files are not things that should be appended to while we are reading them and
        ; while I guess you could have an org file that wouldn't fit into memory that's
        ; the point at which a ... different approach will be needed anyway ... actually
        ; you're going to have to branch on something at some point
        (file-position port
                       (- (file-position port) ; XXX check this
                          (srcloc-span (srcloc-token-srcloc out-raw))))
        (when (debug)
          (println (cons 'sfsfsfsfsfsfsfsfs (file-position port))))
        (let* ([actual-start
                ; not sure exactly why we need add1 here, but we do to
                ; keep things aligned, I may be stepping back too far?
                (add1 (file-position port))]
               [port-end (open-input-string "\n")]
               [this-final-port
                (input-port-append #f
                                   port
                                   port-end)]
               [-final-out ; FIXME we need a way to emit a final newline even if the last token does not need it, because the grammar does
                ; this is because we cannot terminate various parts of the grammar without knowing that we are at eol
                (laundry-lexer this-final-port)]
               #;
               [_ (pretty-print (list '-final-out: -final-out))]
               [final-out
                ;#;
                (fix-srcloc-eof -final-out actual-start (file-position port-end))
                #; ; FIXME broken because it doesn't check token-struct-type
                (if (equal? (token-struct-val (srcloc-token-token out-raw))
                            (token-struct-val (srcloc-token-token -final-out)))
                    out-raw
                    (fix-srcloc-eof -final-out actual-start))])
          (when (debug)
            (pretty-print (list 'final-out final-out))
            (println (list 'fffffffffffffffffff
                           (srcloc-token-srcloc out-raw)
                           (srcloc-token-srcloc final-out)
                           )))
          ; nearly final out
          (final-port this-final-port)
          final-out))
      (begin
        #;
        (when (debug)
          (println (cons 'lll-src-loc-source-line-column-position-span out-raw)))
        (if (eq? out-raw eof)
            (let ([fp (final-port)])
              (if fp
                  (let ([out-raw-fp (laundry-lexer fp)])
                    (if (eq? out-raw-fp eof)
                        out-raw-fp
                        (parameterize ([final-port #f]) ; prevent infinite recursion
                          (internal-rest fp out-raw-fp))))
                  out-raw))
            (fix-srcloc-mod out-raw)))))

(define (process-rest port)
  (let* ([out-raw (laundry-lexer port)]
         #;
         [__ (println (cons 'aaaaaaaaaaa out-raw))]
         [out (internal-rest port out-raw)])
    #;
    (when (debug)
      (pretty-print (list 'process-rest: out)))
    out))

(define (laundry-make-tokenizer port)
  (define bof (= (file-position port) 0)) ; XXX outside no monkey w/ port
  ; TODO figure out how to chain lexers essentially: if something other than a token is
  ; returned or rather, if a specific token is returned that is in the set of tokens that
  ; have been defined to have sub-lexers then current-sublexer should be set and called
  ; until it hits eof, and in those cases we probably want to parse the the lexeme
  ; directly because it is already in memory, once we do this we can get rid of the stupid
  ; inverted expander behavior where we have to reassemble everything I think that this is
  ; likely to be the right design tradeoff because then if we want to split the grammars
  ; we can, and do the parsing in parallel, or we can use the grammars merged, and do the
  ; parsing sequentially, the only case where we really have nested grammars is with the
  ; paragraph parser for dealing with the markup the heading parser and the source block
  ; parser are then seen to be modular subgrammars rather than nested subgrammars

  ; in theory, with this approach we can also decide what the correct behavior is for
  ; reparsing malformed blocks, my inclination would be to treat them as plain text and
  ; export them in red or something like that but this would need some consideration,
  ; though most of the control sequences rarely if ever appear in plain text

  (define (next-token)
    ; FIXME case where first token is newline but the tokenizer doesn't need it but parser might
    ; this appears to throw off counts, but it doesn't actually I think? you just have a bit of
    ; weirdness for the very first element, BUT it throws off some span calculations
    ; FIXME case where bof and eof on the same line
    #;
    (println (cons 'lolololol (file-position port)))
    (let ([out (if bof ; sigh branches instead of rewriting
                   (begin
                     (set! bof #f)
                     (process-first port))
                   (process-rest port))])
      (when (debug)
        (pretty-print (list "laundry next-token:" out)))
      out))
  next-token)

(module+ test-port
  (define port (open-input-string "sigh\nhello\n* there"))
  (define prefix-port (open-input-string "\n"))
  (define first-time-port (input-port-append #f prefix-port prefix-port port))
  (define tport (open-input-string "* Hello world\nand now for\n\nsomething")) ; need to end with a paragraph that is not safe
  (define ll (laundry-make-tokenizer #;make-laundry-lexer tport))
  (ll) (ll) (ll) (ll) (ll)
  )

(define section-lexer
  (lexer-srcloc
   [section (token 'SECTION lexeme)]
   [(from/stop-before any-char heading) ; should only fire once
    (token 'ZEROTH lexeme)]))
; random thought: this could be converted to a purely line based first
; pass IF AND ONLY IF every top level expression in the grammar ends
; with a newline or eof and can freely transition to any other line
; type or backtrack

#;
(define (make-section-tokenizer port)
  next-token
  )

(module+ test-markup

  (define (dotest str)
    (let ([next-token (laundry-make-tokenizer (open-input-string str))])
      (next-token)
      (next-token)))

  (dotest " =hello= ")
  (dotest "=hello= ")
  (dotest "=hello=")

  )
