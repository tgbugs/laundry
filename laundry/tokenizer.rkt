#lang racket/base
(define debug (make-parameter #f))
(require racket/pretty)
(require brag/support
         "lex-abbrev.rkt"
         (only-in "parameters.rkt" export-with-sub-superscripts)
         (only-in racket/string string-suffix? string-trim string-split)
         (only-in racket/port input-port-append peeking-input-port)
         (only-in racket/list cons? last)
         (for-syntax racket/base
                     syntax/parse
                     (only-in racket/list combinations permutations)
                     ))
(provide laundry-make-tokenizer
         table-make-tokenizer
         paragraph-make-tokenizer
         bind-runtime-todo-keywords
         get-block-type
         set-port-next-location-from
         (rename-out [debug laundry-tokenizer-debug]
                     [final-port laundry-final-port])
         get-tokens ; debug only
         )

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

(define (make-block-lexer block-type)
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
             (let ([was-heading (regexp-match #rx"\n[*]+$" lexeme)]
                   [no-more (regexp-match #px"^[[:space:]]*$" lexeme)])
               ; TODO there are two ways that hitting a heading can
               ; proceed one is to back up past the last newline and
               ; mark everything in between as malformed, turning into
               ; paragraph, or follow the elisp implementation
               ; behavior and parse going forward treating the block
               ; start line as malformed but paragraph and the rest as
               ; whatever it would parse to at top level, will need to
               ; explore the tradeoffs
               (and (not was-heading) (not no-more) lexeme))
             ])])
    #;
    (log-error "make-lexer stx: ~a" stx)
    (eval-syntax stx (namespace-anchor->namespace nsa))))

(define block-lexers (make-hash))

(define (get-block-lexer block-type)
  (hash-ref! block-lexers block-type (λ () (make-block-lexer block-type))))

(define (set-port-next-location-from src dest)
  (define-values (line col pos) (port-next-location src))
  (set-port-next-location! dest line col pos))

(define (peek-stop TOKEN lexeme input-port lexer-more #:malformed [malformed #f])
  "lexer-more should return lexeme-more on a good match or #f on a bad
match to avoid the need for an additional predicate, that information
should be statically encoded in the lexer"
  ; more consisten
  (let* ([peek-port (peeking-input-port input-port #:init-position (+ 1 (file-position input-port)))]
         [_ (port-count-lines! peek-port)]
         [lexeme-more? (lexer-more peek-port)]
         #;
         [lexeme-combined (string-append lexeme lexeme-more)]
         #;
         [ok (good? lexeme-more)]
         #;
         [stop-offset (stop? lexeme-more)]
         )
    (if lexeme-more? ;stop ; TODO do we just keep going or do we mark all the rest as potentially malformed?
        ; I would prefer to mark as malformed and not reparse the peek
        ; XXX no, actually, it is more consistent with the elisp and it is easier to
        ; implement the version where when you peek forward if you match you move the
        ; input port forward, if you miss, just keep lexing and mark as malformed
        ; basically this is the arbitrary lookahead lexer
        (let ([port-position-combinator
               (λ (in)
                 ; location and position are independent and both must be set
                 (file-position in (file-position peek-port))
                 (set-port-next-location-from in peek-port))]
              [tok (token TOKEN (string-append lexeme lexeme-more?))])
          ; TODO update input-port position
          (if (or (file-stream-port? input-port)
                  (string-port? input-port))
              (begin
                (port-position-combinator input-port)
                tok)
              (cons tok port-position-combinator)))
        (let ([tok (token
                    (string->symbol (if malformed (format "~a-MALFORMED" (symbol->string TOKEN)) TOKEN))
                    lexeme)]
              #;
              [comb (λ (in)
                      (file-position in (- (file-position in) offset))
                      (set-port-position! in offset #:back-line #t))])
          tok
          #; ; no reset needs to be done on this branch because we just keep going and parse as if
          ; it were just more of the org file
          (if (or (file-stream-port? input-port)
                  (string-port? input-port))
              (begin
                (comb input-port)
                tok)
              ; XXX resolved down in -stt below FIXME naming and action at a distance
              (cons tok comb))))
    )
  )

(define (set-port-position! input-port offset #:back-line [back-line #f])
  "do it in string chars not in bytes"
  (let*-values ([(l c p) (port-next-location input-port)]
                [(n-l n-c n-p)
                 (values
                  (if back-line (sub1 l) l)
                  (and (not back-line) (- c offset))
                  (- p offset))])
    (set-port-next-location! input-port n-l n-c n-p)))

(define (token-back-1 TOKEN lexeme input-port)
  ; FIXME multi-byte case
  ; FIXME the real solution is to switch to a peeking port so that
  ; we don't have to bother with this nonsense, unfortunately for
  ; the markup case that means we have to peek every time we hit
  ; a potential start case for the markup which is a pain but maybe
  ; still worth it
  (file-position input-port (sub1 (file-position input-port)))
  (let*-values ([(l c p) (port-next-location input-port)]
                [(back-line) (regexp-match #rx"\n$" lexeme)]
                [(n-l n-c n-p)
                 (values
                  (if (and l back-line) (sub1 l) l)
                  (and c (not back-line) (sub1 c))
                  (and p (sub1 p))
                  )]
                [(out) (token TOKEN (substring lexeme 0 (sub1 (string-length lexeme))))]
                )
    (set-port-next-location! input-port n-l n-c n-p)
    #;
    (log-error "token-back-1 out: ~s ~s" out lexeme)
    out)
  #;
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
                                offset))])
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
                (println (list 'p1: (file-position input-port)))
                (println (list 'pnl1: (let-values ([(l c p) (port-next-location input-port)]) (list l c p))))
                (println (list 'pppppppppppppppp (peek-char input-port)))
                (println (list 'tttttttttttttttt token-correct)))
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
              ":"
              (:* (:or " " "\t"))
              "\n")
        (token 'TAGS lexeme)]
       ["COMMENT" (token 'CHARS-COMMENT lexeme)] ; this must come befor RTK
       [(:or #,@keywords) (token 'RUNTIME-TODO-KEYWORD lexeme)]
       [(:seq "[#" (:or 0-9 alpha) "]") (token 'PRIORITY lexeme)] ; lower case allowed but it is upcased internally (base36)
       [(:or " " "\t") (token 'BLANK lexeme)] ; unfortunately we still have to split on space
       [(:seq (:+ (:~ #;"*" "[" "]" ":" "\n" " " "\t")))
        (token 'OTHER lexeme)]
       ["\n" (token 'NEWLINE-END lexeme)]
       [(:~ (:or " " "\t" "\n")) ; insurance
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
  (require laundry/grammar/heading)
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

(define-syntax (paragraph-lexer-common stx)
  (syntax-parse stx
    [(_ body ...)
     #'(lexer-srcloc

        ; lexer specific forms

        body ...

        ;;; common

        ["[" (token 'LSB lexeme)] ["]" (token 'RSB lexeme)]
        ["{" (token 'LCB lexeme)] ["}" (token 'RCB lexeme)]
        ["(" (token 'LP lexeme)]  [")" (token 'RP lexeme)]
        ["<" (token 'LAB lexeme)]  [">" (token 'RAB lexeme)]

        ["^"
         (if (eq? (export-with-sub-superscripts) #t)
             (token 'HAT lexeme)
             (token 'HAT-SCRIPT-DISABLED lexeme))]

        ["_"
         (if (eq? (export-with-sub-superscripts) #t)
             (token 'UNDERSCORE lexeme)
             (token 'UNDERSCORE-SCRIPT-DISABLED lexeme))]

        ["*" (token 'ASTERISK lexeme)]
        ["/" (token 'SLASH lexeme)]
        ["+" (token 'PLUS lexeme)]
        ["~" (token 'TILDE lexeme)]
        ["=" (token 'EQUALS lexeme)]

        ["@" (token 'AT lexeme)]

        ["\n" (token 'NEWLINE lexeme)]
        [wsnn (token 'WSNN1 lexeme)]

        [(:~ (:or whitespace paragraph-special)) (token 'OTHER lexeme)]


        ; org objects

        [latex-entity/fragment-1 (token 'LATEX-ENTITY-OR-FRAGMENT-1 lexeme)]
        [latex-fragment-n (token 'LATEX-FRAGMENT-N lexeme)]
        [latex-fragment-parens (token 'LATEX-FRAGMENT-PARENS lexeme)]

        [export-snip (token 'EXPORT-SNIP lexeme)]

        [citation (token 'CITATION lexeme)] ; FIXME annoying because it gets parsed twice

        [footnote-anchor (token 'FOOTNOTE-ANCHOR lexeme)]
        [footnote-inline-start (token 'FOOTNOTE-START-INLINE lexeme)]

        [inline-call-start (token 'INLINE-CALL-START lexeme)]
        [inline-src-block-start (token 'INLINE-SRC-BLOCK-START lexeme)]

        [hyperlink (token 'LINK lexeme)]
        [hyperlink-ab (token 'LINK-AB lexeme)]

        [macro-invocation (token 'MACRO lexeme)]

        [noweb-target (token 'NOWEB-TARGET lexeme)]
        [radio-target (token 'RADIO-TARGET lexeme)]

        [stats-percent (token 'STATS-PERCENT lexeme)]
        [stats-quotient  (token 'STATS-QUOTIENT lexeme)]

        [timestamp (token 'TIMESTAMP lexeme)]

        )]))

(define paragraph-lexer-alt
  (paragraph-lexer-common
   [(:>= 2 (:~ paragraph-special "\n" whitespace mu-post-less-whitespace-delim))
    (let ([next-char (peek-char input-port)])
      (if (for/or ([char '(#\space #\newline #\tab
                           #\] #\- #\. #\, #\; #\: #\! #\? #\' #\"
                           )]) (char=? next-char char))
          (token 'OTHERN lexeme)
          (token-back-1 'BEFORE-BEFORE-SPECIAL lexeme input-port)))]

   [superscript-start-b ; massively simplifies the grammar
    (if (export-with-sub-superscripts)
        (token 'SUP-START-B lexeme)
        (token 'SCRIPT-DISABLED lexeme))]
   [subscript-start-b ; massively simplifies the grammar
    (if (export-with-sub-superscripts)
        (token 'SUB-START-B lexeme)
        (token 'SCRIPT-DISABLED lexeme))]

   ; this does not parse markup ? because ? ... no that can't quite be right
   ; I think there is a variant that does parse markup ... ?
   ; it would be amazing if there were only 3 contexts needed to account for all of the org paragraph
   ; syntax at the level of the lexer ... we'll see if it pans out
   ))

(define paragraph-lexer-alt-markup
  (paragraph-lexer-common
   [(:>= 2 (:~ paragraph-special "\n" whitespace mu-post-less-whitespace-delim))
    (let ([next-char (peek-char input-port)])
      (if (for/or ([char '(#\space #\newline #\tab
                           #\] #\- #\. #\, #\; #\: #\! #\? #\' #\"
                           )]) (char=? next-char char))
          (token 'OTHERN lexeme)
          (token-back-1 'BEFORE-BEFORE-SPECIAL lexeme input-port)))]

   [markup-* (token-back-1 'BOLD lexeme input-port)]
   [markup-/ (token-back-1 'ITALIC lexeme input-port)]
   [markup-_ (token-back-1 'UNDERLINE lexeme input-port)] ; FIXME handle the intersection of mu-pre and subscript-pre
   [markup-+ (token-back-1 'STRIKE lexeme input-port)]

   [markup-= (token-back-1 'VERBATIM lexeme input-port)]
   [markup-~ (token-back-1 'CODE lexeme input-port)]

   ))

(define paragraph-lexer
  (paragraph-lexer-common
   [(:>= 2 (:~ paragraph-special "\n")) ; need newline to simplify adding and removing newlines during parsing (ick)
    (if (char=? (peek-char input-port) #\newline)
        (token 'OTHERN lexeme)
        (token-back-1 'BEFORE-BEFORE-SPECIAL lexeme input-port))]

   ; markup

   ; XXX we may still need the -NP -PA split
   ; or we can work around the issue by checking for unmached
   ; in a second pass on nested in the expander if we REALLY have to
   [markup-* (token-back-1 'BOLD lexeme input-port)]
   [markup-/ (token-back-1 'ITALIC lexeme input-port)]
   [markup-_ (token-back-1 'UNDERLINE lexeme input-port)] ; FIXME handle the intersection of mu-pre and subscript-pre
   [markup-+ (token-back-1 'STRIKE lexeme input-port)]

   [markup-= (token-back-1 'VERBATIM lexeme input-port)]
   [markup-~ (token-back-1 'CODE lexeme input-port)]

   ))


(define paragraph-lexer-no-markup
  (paragraph-lexer-common
   [(:>= 2 (:~ paragraph-special "\n")) ; need newline to simplify adding and removing newlines during parsing (ick)
    (if (char=? (peek-char input-port) #\newline)
        (token 'OTHERN lexeme)
        (token-back-1 'BEFORE-BEFORE-SPECIAL lexeme input-port))]

   [superscript-start-b ; massively simplifies the grammar
    (if (export-with-sub-superscripts)
        (token 'SUP-START-B lexeme)
        (token 'SCRIPT-DISABLED lexeme))]
   [subscript-start-b ; massively simplifies the grammar
    (if (export-with-sub-superscripts)
        (token 'SUB-START-B lexeme)
        (token 'SCRIPT-DISABLED lexeme))]

   ))

; FIXME TODO consider using parameters to avoid emitting duplicate
; markup but consider also that this may not be the best or even
; correct place to attempt such a thing

#;
(define paragraph-lexer-old

  ; TODO peeking markup
  ; we don't have to peek on every single char, we only have to peek on
  ; whitespace and mu-pre, specifically #\newline #\space #\Tab #\" #\' #\- #\{ #\(
  ; and actually we only have to peek on #\" #\' #\- #\{ #\(, in all other cases
  ; we never match markup and continue

  ; XXX the pay no attention to me variants go a long way to reducing the most common quadratic slowdowns
  ; however it is clear that the paragraph grammar is still broken
  (lexer-srcloc ; FIXME vs lexer-src-pos ????

   [(:>= 2 (:~ paragraph-special))
    (token-back-1 'BEFORE-BEFORE-SPECIAL lexeme input-port)
    ; then what you do is you have a bunch of these BBS blocks
    ; separated by 2 chars and then another BBS block, then you
    ; can build the logical of the grammar using the pairs of
    ; chars, that seems like it may be tractable
    ; the markup is still a nightmare in this situation though :/
    ; FIXME nope ... not so simple, because there are a ton of
    ; different rules for when you need to terminate a token ;_;
    ; the one that really screws everything up is whitespace
    ; that might be the ONE state that we track ... whether to
    ; break on whitespace ... hrm
    ; also the markup post bit makes this naieve appraoch seem
    ; very unlikely to work ...

    ; or we rewrite the paragraph grammar to try to avoid the
    ; quadratic pitfalls and split everything on whitespace
    ; except for the whole lines that contain nothing of interest
    ; lbrc would remain the basis for stretches of text that were
    ; uninteresting, but we would do it line by line only breaking
    ; on newlines insteaad of all whitespace for the boring cases
    ]

   #;
   [(:+
     (:~ mu-check mu-marker )
     (:seq mu-marker)
     )
    (token 'LOL lexeme)
    ]

   #; ; not quite
   [(:seq (:+ (:~ mu-marker mu-check #;(and some other stuff))))
    (token-back-1 'PAY-NO-ATTENTION-TO-ME-Z lexeme input-port)]

   [(:seq
     (:+
      (:or
       (:~ little-black-raincloud)
       (:seq
        (:~ (:or whitespace mu-check mu-marker "[" "<" "@" "^"))
        (:or "*" "/" "+" "=" "~")) ; mu-marker not underline (because script)
       ))
     ; we can't stop for all elements of little black raincloud, due to markup being in there
     ; we need a token back 2 variant for markup
     (:or "\n" "[" "]" "{" "}" "<" #;">" "@" "^"))
    (token-back-1 'PAY-NO-ATTENTION-TO-ME lexeme input-port)]
   [(:seq (:+ (:~ little-black-raincloud))
          wsnn+
          (:or "_" "~" "=" "/" "*" "+"))
    (token-back-1 'PAY-NO-ATTENTION-TO-ME-WHITESPACE lexeme input-port)]
   [(:seq (:+ (:~ little-black-raincloud))
          mu-pre-safe-1
          (:or "_" "~" "=" "/" "*" "+"))
    (token-back-1 'PAY-NO-ATTENTION-TO-ME-MU-PRE-SAFE lexeme input-port)]
   [(:seq (:+ (:~ little-black-raincloud))
          (:~ (:or whitespace mu-pre-safe-1 mu-marker "}" "]" ">" "@" "^"))
          (:or "_" "~" "=" "/" "*" "+"))
    (token-back-1 'PAY-NO-ATTENTION-TO-ME-NOT-MU lexeme input-port)]

   #;
   [(:seq (:+ (:~ (:or "\n" "[" "]" "{" "}" #;"(" #;")" "_" "^" "~" "=" "/" "*" "+" "<" ">" "@"))) "]")
    (token-back-1 'PAY-NO-ATTENTION-TO-ME lexeme input-port)]

   #; ; don't bother right now ; XXX apparently having this and the one above can cause a nearly infinite hang !?
   [(:seq (:* (:~ (:or "\n" "[" "]" "{" "}" "(" ")" "_" "^" "~" "=" "/" "*" "+" "<" ">" "@"))) mu-pre-safe)
    (token-back-1 'MU-PRE-SAFE lexeme input-port)]

   [wsnn+ (token 'WSNN lexeme)]
   ["\n" (token 'NEWLINE lexeme)]
   ["[" (token 'LSB lexeme)] ; this cannot be parsed by itself due to ambiguity with [=hello=]\nlol (=oops=)
   ["]" (token 'RSB lexeme)]
   ["{" (token 'LCB lexeme)] ["}" (token 'RCB lexeme)]
   ["(" (token 'LP lexeme)]  [")" (token 'RP lexeme)]
   [(:seq "[" "]") (token 'LSB-RSB lexeme)]
   [(:seq "{" "}") (token 'LCB-RCB lexeme)]
   [(:seq "(" ")") (token 'LP-RP lexeme)]
   ["_" (token 'UNDERSCORE lexeme)]
   ["^" (token 'HAT lexeme)]

   [mu-pre-safe (token 'MU-PRE-SAFE lexeme)]

   [(:seq (:+ (:~ mu-pre-1 mu-marker #;mu-post-1 "[" "]" "}" ")" script-marker whitespace))
          ; FIXME matches [fn:: ; so we have to exclude [ from starting, we have have to exclude it later too
          ; XXX not entirely sure that mu-post-1 should be in here? maybe for the first
          ; but we peek mu-post-1 now
          ; make sure that we eat any markers that are preceeded by non whitespace
          (:* (:~ mu-pre-1           #;mu-post-1 "]" "}" ")" script-marker whitespace)))
    (token 'STUFF-B lexeme)]
   [(:+ (:or (:& (:~ mu-pre-safe-1 "[" "]" "}" ")" script-marker whitespace) mu-post-less-whitespace-delim)
             ; have to exclude [ so that LCB works correctly I think, may need to split as we did above
             ; but both [ and ( can occur effectively in the middle of things so tricky
             ; can't quite be mu-marker becuse _ is also subscript
             "=" "~" "*" "/" "+"))
    (token 'STUFF-A lexeme)]
   #;
   [(:seq "[" (:* mu-marker) "_" (:~ (:or whitespace "{" "}" "[" "]" "<" ">" "@" "^" mu-marker)))
    (token 'LSB-MU-MARKER lexeme)]
   ;#; ; there is no no hope for this approach, something always breaks or slips out, the problem is underscore
   ; I think it has to be parsed by itself, and the underline markup has to be handled in a different way
   ; nesting rules will simply break otherwise ...
   [(:&
     (from/stop-before
      (:seq "[" (:* mu-marker) "_")
      (:or whitespace "{" "}" "[" "]" "(" ")" "<" ">" "@" "^"
           ; script start ; FIXME I think this is probably missing from little black raincloud
           "+" "-" "," "." "\\" alpha 0-9 ; FIXME alphabetic numeric has things like @ in it !? ; FIXME we HAVE to stop before these or it overruns
           ))
     (:seq any-string "_"))
    ; explicitly catch these cases, back up 1 and let UNDERLINE-AMBIG take over from there
    (if (= (string-length lexeme) 1)
        (token 'LSB lexeme)
        (token-back-1 'LSB-MU-MARKER lexeme input-port))
    #;
    (let ([next-char (peek-char input-port)])
      (if (for/or ([not-ss '(#\space #\newline #\tab #\[ #\] #\{ #\} #\( #\) #\< #\> #\@ #\^)]) (char=? next-char not-ss))
          (if (= (string-length lexeme) 2)
              (token 'LSB-UNDERSCORE lexeme) ; FIXME need to differentiate script start case from the others
              (token-back-1 'LSB-MU-MARKER lexeme input-port))
          (if (= (string-length lexeme) 2)
              (token 'LSB-UNDERSCORE-BEFORE-SCRIPT lexeme) ; FIXME need to differentiate script start case from the others
              (token-back-1 'LSB-MU-MARKER-BEFORE-SCRIPT lexeme input-port))))]
   #;
   [(:&
     (from/stop-before
      (:seq "[" (:* mu-marker) "_")
      (:or "+" "-" "," "." "\\" alpha 0-9)) ; XXX apparently alphabetic and/or numeric includes @ !?!??!
     (:seq any-string "_"))
    (if (= (string-length lexeme) 2)
        (token 'LSB-UNDERSCORE-BEFORE-SCRIPT lexeme)
        (token-back-1 'LSB-MU-MARKER-BEFORE-SCRIPT lexeme input-port))]
   [(:seq "["
          (:or
           (:+ mu-marker-less-_)
           #; ; can't even do this because [/*__+.x=+__/* needs to parse as script disabled
           (:seq mu-marker-less-_ (:* mu-marker) mu-marker-less-_) ; FIXME ends in _{} or _asdf case
           ;(from/stop-before (:+ "_") (:~ (:or "{" "]")))
           #; ; [/*_{} sigh_ dooms us here ?? no, we might be ok? because of underline-ambig ?!?
           (:seq mu-marker-less-_ (:* mu-marker) "_"
                 ; FIXME this is really tricky, I'm not sure we can safely handle it
                 (:~ (:or "{" "[" "]" "@" "<" whitespace))
                 )
           #;
           (:seq "_"
                 (:or
                  (:+ mu-marker)
                  (:~ (:or "{" "]" whitespace)) ; don't eat whitespace basically
                  )
                 )
           ;(:+ (:or "*" "+" "/" "=" "~"))
           #;
           (:seq  (:* (:or "*" "+" "/" "=" "~")))
           #;
           (:seq (:+ "_") (:~ (:or "{" "]" whitespace)))))
    ; TODO see if we need to handle underscore (we do)
    (token 'LSB-MU-MARKER lexeme)]

   #;
   [(:seq "[" "_" (:or "+" "-" "," "." "\\" alphabetic numeric))
    ; we only need to handle the single underline case, the stop before variant below catches the rest for us
    ; and it only fails to handle this one because it needs token-back-1
    ; FIXME nasty processing required in expander if we try this ???
    (token 'LSB-SCRIPT-START lexeme)]

   #; ; no longer needed due to pay no attention to me XXX FALSEish
   [(:seq "[" ; ensure that markup doesn't run wild with an incorrect parse
          (:or
           ;"=" "~" "*" "/" "+"
           (:~
            (:or
             whitespace
             "_" "^" ; FIXME potential issues if script export is enabled/disabled
             "[" "]" ; =lol [= case will match markup so not an issue
             "{" "}"
             "(" ")"
             ))))
    (token 'LSB-PLUS lexeme)]
   [(:seq ; interactions between markup and pb-script
     script-marker
     ; FIXME this matches __ which breaks __{sub}
     ; FIXME this matchs _@ too, which breaks [_@@html: html@@_]
     (:~ (:or script-marker "@" "[" "{" "(" "*" "+" "-" alpha 0-9 "," "." "\\" whitespace))
     #;
     (:+ (:&
          (:~ (:or "{" "(" "*" "+" "-" alpha 0-9 "," "." "\\" ))
          (:or (:& (:~ mu-pre-1 "]" "}" ")" "^" "_") mu-post-less-newline) mu-marker))))
    (token 'STUFF-C lexeme)]

   #|
   [markup-* (token 'BOLD lexeme)]
   [markup-/ (token 'ITALIC lexeme)]
   [markup-_ (token 'UNDERLINE lexeme)]
   [markup-+ (token 'STRIKE lexeme)]

   [markup-= (token 'VERBATIM lexeme)]
   [markup-~ (token 'CODE lexeme)]
   |#

   [markup-=-nopar (token-back-1 'VERBATIM-NP lexeme input-port)]
   [markup-~-nopar (token-back-1 'CODE-NP lexeme input-port)]

   [markup-*-nopar (token-back-1 'BOLD-NP lexeme input-port)]
   [markup-/-nopar (token-back-1 'ITALIC-NP lexeme input-port)]
   [markup-_-nopar (token-back-1 'UNDERLINE-NP lexeme input-port)]
   [markup-+-nopar (token-back-1 'STRIKE-NP lexeme input-port)]


   [markup-=-paren (token-back-1 'VERBATIM-PA lexeme input-port)]
   [markup-~-paren (token-back-1 'CODE-PA lexeme input-port)]

   [markup-*-paren (token-back-1 'BOLD-PA lexeme input-port)]
   [markup-/-paren (token-back-1 'ITALIC-PA lexeme input-port)] ; might need to zap this one too to avoid issues with hyperlinks?
   #; ; all the others we can handle, this one gobbles script
   [markup-_-paren (token-back-1 'UNDERLINE-PA lexeme input-port)]
   [markup-+-paren (token-back-1 'STRIKE-PA lexeme input-port)]


   [markup-= (token-back-1 'VERBATIM lexeme input-port)]
   [markup-~ (token-back-1 'CODE lexeme input-port)]

   ; #| ; duh we don't have to back up I fixed it in the pattern itself
   ; sadly we DO have to back up due to cases like x_{y}_{z}
   [markup-* (token-back-1 'BOLD lexeme input-port)]
   [markup-/ (token-back-1 'ITALIC lexeme input-port)]
   [markup-_-ok (token-back-1 'UNDERLINE lexeme input-port)] ; NASTY interaction with subscript
   ; FIXME may need not allow parens in markup in cases like this? urg?
   ; FIXME this can gobble parens for paren matching, which have to be matched if it isn't actually underline
   [markup-_-ambig (token-back-1 'UNDERLINE-AMBIG lexeme input-port)] ; I do NOT like how this is handled, but lookbehind seems worse
   [markup-+ (token-back-1 'STRIKE lexeme input-port)]

   [markup-= (token-back-1 'VERBATIM lexeme input-port)]
   [markup-~ (token-back-1 'CODE lexeme input-port)]
   ; |#


   #;
   [mu-pre-n-not-lcb (token 'MU-PRE-N-NOT-LCB lexeme)] ; LCB cannot be in mu-pre-n because it would block macro
   #;
   [mu-pre-1 (token 'MU-PRE-1 lexeme)] ; needed to prevent accidental capture when :+ length for stuff is longer than markup

   ; org objects

   [latex-entity/fragment-1 (token 'LATEX-ENTITY-OR-FRAGMENT-1 lexeme)]
   [latex-fragment-n (token 'LATEX-FRAGMENT-N lexeme)]
   [latex-fragment-parens (token 'LATEX-FRAGMENT-PARENS lexeme)]

   [export-snip (token 'EXPORT-SNIP lexeme)]

   [citation (token 'CITATION lexeme)] ; FIXME annoying because it gets parsed twice

   [footnote-anchor (token 'FOOTNOTE-ANCHOR lexeme)]
   [footnote-inline-start (token 'FOOTNOTE-START-INLINE lexeme)]

   #; ; tokenizer can't do this the spec is ... inaccurate
   [inline-call (token 'INLINE-CALL lexeme)]
   [inline-call-start (token 'INLINE-CALL-START lexeme)]

   #;
   [inline-src-block (token 'INLINE-SRC-BLOCK lexeme)]
   [inline-src-block-start (token 'INLINE-SRC-BLOCK-START lexeme)]

   [hyperlink (token 'LINK lexeme)]
   [hyperlink-ab (token 'LINK-AB lexeme)]

   [macro-invocation (token 'MACRO lexeme)]

   [noweb-target (token 'NOWEB-TARGET lexeme)]
   [radio-target (token 'RADIO-TARGET lexeme)]

   [stats-percent (token 'STATS-PERCENT lexeme)]
   [stats-quotient  (token 'STATS-QUOTIENT lexeme)]

   ; FIXME checking parameters like this doesn't need to be done here
   ; I don't think there are any cases where script being off would
   ; lead to a difference in the parse so fundamental that we could
   ; not recover the correct value, on the other hand, this way we
   ; don't have to worry about it at all ...
   [superscript ; this is the non ^{} case
    (peek-script-disabled 'SUPERSCRIPT lexeme input-port)]
   [superscript-start-b
    (if (export-with-sub-superscripts)
        (token 'SUP-START-B lexeme)
        (token 'SCRIPT-DISABLED lexeme))]
   #;
   [superscript-start-p
    (if (export-with-sub-superscripts)
        (token 'SUP-START-P lexeme)
        (token 'SCRIPT-DISABLED lexeme))]

   [subscript ; this is the non _{} case
    (peek-script-disabled 'SUBSCRIPT lexeme input-port)]
   [subscript-start-b
    (if (export-with-sub-superscripts)
        (token 'SUB-START-B lexeme)
        ; FIXME check to see if _{} is included verbatim
        (token 'SCRIPT-DISABLED lexeme))]
   #;
   [subscript-start-p
    (if (export-with-sub-superscripts)
        (token 'SUB-START-P lexeme)
        (token 'SCRIPT-DISABLED lexeme))]

   [timestamp (token 'TIMESTAMP lexeme)]

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

(define (peek-script-disabled type lexeme input-port)
  (if (eq? (export-with-sub-superscripts) #t)
      (token type lexeme)
      (let ([next-char (peek-char input-port)])
        (when (debug)
          (pretty-write (list 'paragraph-lexer-subscript-1: next-char)))
        (if (for/or ([mu '(#\~ #\= #\/ #\* #\+)]) (char=? next-char mu)) ; XXX note we skip _ because we handle that ambig elsewhere
            ; FIXME surely read-char fails to update the srcloc
            (token 'SCRIPT-DISABLED (string-append lexeme (string (read-char input-port))))
            (token 'SCRIPT-DISABLED lexeme)))))

(module+ test-paragraph
  ; FIXME terminal double newline never makes it through the top level lexer
  (debug #t)
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

  (let* ([port (open-input-string " *bo ld* ")]
         [next-token (paragraph-make-tokenizer port)])
    (list (next-token) (next-token) (next-token)))

  (let* ([port (open-input-string " *bo ld* more")]
         [next-token (paragraph-make-tokenizer port)])
    (list (next-token) (next-token) (next-token) (next-token) (next-token)
          (next-token) (next-token) (next-token) (next-token)))

  (let* ([port (open-input-string " *bo ld*:more")]
         [next-token (paragraph-make-tokenizer port)])
    (list (next-token) (next-token) (next-token) (next-token) (next-token)
          (next-token) (next-token) (next-token) (next-token)))

  (let* ([port (open-input-string " *bo ld*!more")]
         [next-token (paragraph-make-tokenizer port)])
    (list (next-token) (next-token) (next-token) (next-token) (next-token)
          (next-token) (next-token) (next-token) (next-token)))

  (let* ([port (open-input-string " *bo ld*,more")]
         [next-token (paragraph-make-tokenizer port)])
    (list (next-token) (next-token) (next-token) (next-token) (next-token)
          (next-token) (next-token) (next-token) (next-token)))

  (let* ([port (open-input-string " *bo ld*mo re")]
         [next-token (paragraph-make-tokenizer port)])
    (list (next-token) (next-token) (next-token) (next-token) (next-token)
          (next-token) (next-token) (next-token) (next-token)))

  (let* ([port (open-input-string "oops")]
         [next-token (paragraph-make-tokenizer port)])
    (list  (next-token)   (next-token)
             ))

  )

(define table-lexer-nlf
  (lexer-srcloc
   [(:seq "\n" (:* (:or "\t" " ")) "|" "-" (:* (:~ "\n")))
    (token 'TABLE-ROW-RULE lexeme)]
   [(:seq "\\" "|") (token 'ESC-PIPE "|")]
   ["|" (token 'PIPE lexeme)]
   #; ; gobbles the newline needed by table row ... sigh
   [(:seq "|" "\n") (token 'PIPE-FINAL lexeme)]
   [(:seq "\n" wsnn+) (token 'NLWS lexeme)] ; beat the quadratic this way
   ["\n" (token 'NEWLINE lexeme)]
   [wsnn+ (token 'WSNN1-N lexeme)]
   [(:+ (:~ (:or "|" "\\" "\t" " " "\n"))) (token 'REST lexeme)]))

(define table-lexer-nll
  (lexer-srcloc
   [(:seq "|" "-")
    (token 'TABLE-ROW-RULE-START lexeme)]
   [(:seq "\\" "|") (token 'ESC-PIPE "|")]
   ["|" (token 'PIPE lexeme)]
   [(:seq "|" "\n") (token 'PIPE-FINAL lexeme)]
   ["\n" (token 'NEWLINE lexeme)]
   [wsnn+ (token 'WSNN1-N lexeme)]
   [(:+ (:~ (:or "|" "\\" "\t" " " "\n"))) (token 'REST lexeme)])
  )

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

(define break-on-alt (make-parameter #f))
(define match-markup? (make-parameter #t))
(define (paragraph-make-tokenizer port)
  (define (next-token)
    (let* ([lexer (if (break-on-alt)
                      (if (match-markup?)
                          paragraph-lexer-alt-markup
                          paragraph-lexer-alt)
                      (if (match-markup?)
                          paragraph-lexer
                          paragraph-lexer-no-markup))]
           [out (lexer port)])
      (if (not (eq? out eof))
          (let* ([token (srcloc-token-token out)]
                 [token-type (token-struct-type token)])
            (if (memq token-type
                      '(
                        LSB RSB
                        LCB RCB
                        LP RP
                        LAB RAB
                        HAT HAT-SCRIPT-DISABLED
                        UNDERSCORE UNDERSCORE-SCRIPT-DISABLED
                        ; markup markers must be included so that we can
                        ; catch markup post without introspecting tokens
                        ASTERISK
                        SLASH
                        PLUS
                        TILDE
                        EQUALS
                        AT

                        ; don't need this because only the occurance of single alt chars
                        ; directly after some of the above matter for this
                        ;OTHERN ; needed to be able to read the alt char by itself?

                        ))
                ; FIXME - ' "
                (let ()
                  (break-on-alt #t)
                  (match-markup? #f)
                  (when (memq token-type '(LCB LP))
                    ; we only need to match markup on these two here
                    ; because all other markup cases are handled below
                      (match-markup? #t)))
                (let ()
                  (break-on-alt #f)
                  (if (memq token-type '(
                                         NEWLINE WSNN1 MU-PRE-SAFE
                                         ; LCB LP ; these two are unreachable here because
                                         ; the other branch will always be selected
                                         ))
                      ; we only need to switch lexer when we hit the BEFORE-SPECIAL slot
                      (match-markup? #t)
                      (match-markup? #f)
                      )
                  ))

            )
        (break-on-alt #f)
        )
      (when (debug) ; XXX (debug) as a parameter doesn't work when called
               ; at syntax time because parameters can't cross phases,
               ; even if syntax time is called at and inside runtime
               ; and/org there are some module boundary issues related
               ; to expanding at runtime that are getting tangled up
        (println (list ':paragraph-make-tokenizer lexer))
        (pretty-print out))
      out))
  next-token)

(define (table-make-tokenizer port)
  (define (next-token)
    (let ([out (table-lexer-nll port)])
      #;
      (begin
        (println ':table-make-tokenizer)
        (pretty-print out))
      out))
  next-token)

(define-namespace-anchor nsa)

(define laundry-lexer
  (lexer-srcloc

   [heading (token 'HEADING lexeme)]
   [planning-line (token 'PLANNING-ELEMENT lexeme)]
   [planning-line-malformed (token 'PLANNING-ELEMENT-MALFORMED lexeme)]

   [(from/stop-before unknown-block-line-end "\n")
    ; if we run into one of these by itself it is malformed because it is disconnect
    (token 'UNKNOWN-BLOCK-MALFORMED lexeme)]
   [(from/stop-before unknown-block-line-begin "\n")
    (peek-stop
     'GREATER-BLOCK
     lexeme
     input-port
     (get-block-lexer (get-block-type lexeme))
     #:malformed #t)]
   [dynamic-block
    ; FIXME the failure mode for dynamic block separation produces well formed keywords
    ; which probably means that there needs to be a way to define keywords so that if they
    ; appear as keywords they indicate an error, dynamic definitions for pairing keywords
    ; might be of interest as part of the self defining keyword spec
    (token 'DYNAMIC-BLOCK lexeme)]
   [table-element
    ; FIXME this still has issues where the newline following the last | is dropped
    (token 'TABLE-ELEMENT lexeme)
    #;
    (token-stop-before-table-double-blank-line 'TABLE-ELEMENT lexeme input-port start-pos)]
   [descriptive-list-line (token 'DESCRIPTIVE-LIST-LINE lexeme)]
   [ordered-list-line (token 'ORDERED-LIST-LINE lexeme)]
   [keyword-element (token 'KEYWORD-ELEMENT lexeme)] ; before hyperlink for #+[[[]]]:asdf

   [(from/stop-before (:seq "\n" wsnn* "#+" (:or "begin" "BEGIN" "end" "END") "_") whitespace)
    (token 'UNKNOWN-BLOCK-MALFORMED lexeme)]
   ; FIXME a bit of ambiguity about how to treat the #+begin_ cases,
   ; atm I differentiate them from keywords since the user probably
   ; mean for them to be blocks, not keywords
   [keyword-element-malformed (token 'KEYWORD-ELEMENT-MALFORMED lexeme)]

   [comment-element (token 'COMMENT-ELEMENT lexeme)]
   [(from/stop-before (:seq "\n" wsnn* "#") "\n") ; FIXME BIG RISK of capturing other elements with this?
    ; FIXME HACK
    ; NOTE empty fixed width elements break up non-empty fixed width elements from the lexer
    ; I think we might be able to handle that via 3 special cases in the abbrevs: start, middle, end
    ; ACTUALLY never mind, it is trivial to handle in the grammar
    (if (regexp-match #px"^\n[[:blank:]]*#$" lexeme)
        (token 'COMMENT-ELEMENT lexeme)
        (token 'PARAGRAPH lexeme))]

   [fixed-width-element (token 'FIXED-WIDTH-ELEMENT lexeme)]
   [(from/stop-before (:seq "\n" wsnn* ":") "\n") ; FIXME BIG RISK of capturing other elements with this?
    ; FIXME HACK
    (if (regexp-match #px"^\n[[:blank:]]*:$" lexeme)
        (token 'FIXED-WIDTH-ELEMENT lexeme)
        (token 'PARAGRAPH lexeme))]

   [drawer-props
    (token 'DRAWER-PROPS lexeme)]
   [drawer-ish
    (token 'DRAWER lexeme)]

   [paragraph (token 'PARAGRAPH lexeme)]

   [(:>= 2 "*") (token 'STARS lexeme)] ; need this in lexer otherwise performance tanks in the parser
   ["*" (token 'ASTERISK lexeme)]
   #; ; I don't think we actually need this ? it is sort of a sentinel for other issues with holes in the grammar though?
   ["-" (token 'HYPHEN-OOPS lexeme)] ; apparently there are some cases where this can fall through ?
   ["\n" (token 'NEWLINE lexeme)]
   #;
   [" " (token 'SPACE)]
   #; ; TODO should help with perf
   [(:+ " ") (token 'SPACE-N lexeme)]
   #;
   ["\t" (token 'TAB)]
   [wsnn+ (token 'WSNN1-N lexeme)]

   [footnote-definition
    ; FIXME isn't this stop before broken or did I fix this in some other way at some point?
    ; maybe I did it my introspecting the span vs the current port position or something !? no?
    (token 'FOOTNOTE-DEFINITION lexeme)]

   [(eof) (return-without-srcloc eof)]

   [":" (token 'COLON lexeme)]
   ["[" (token 'LSB lexeme)]
   ["]" (token 'RSB lexeme)]

   [0-9 (token 'DIGIT lexeme)]
   [(:>= 2 0-9) (token 'DIGIT-N lexeme)]

   [alpha (token 'ALPHA lexeme)] ; FIXME TODO alpha+ ?
   [(:>= 2 alpha) (token 'ALPHA-N lexeme)]

   [(:+ negated-set) (token 'NEGATED-SET lexeme)]))

(module+ test-laundry-lexer
  ; two paragraphs here !? malformed triggering too quickly ??
  ; I think this is because we have a paragraph-2 in the second position somehow?
  ; yes, that is why, there isn't a way around this right now
  (laundry-lexer (open-input-string "\n[fn:: sigh\nwhat\n"))
  (laundry-lexer (open-input-string "\nsigh\nwhat\n"))

  )

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
         [srcloc-token-out (do-make-srcloc-token token srcloc)])
    (when (debug)
      (println (list "fix-srcloc-mod out:" srcloc-token-out)))
    srcloc-token-out))

(define (do-make-srcloc-token tok srcloc)
  "srcloc-token's don't show up in the debug info embed the location information in just the token"
  ; FIXME the lines etc. on this is OBVIOUSLY wrong
  ; 14 lines off in cursed.org
  (make-srcloc-token
   (token-struct
    (token-struct-type tok)
    (token-struct-val tok)
    (srcloc-position srcloc)
    (srcloc-line srcloc)
    (srcloc-column srcloc)
    (srcloc-span srcloc)
    #f)
   srcloc))

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
         [srcloc-token-out (do-make-srcloc-token token srcloc)])
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
          (do-make-srcloc-token
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
    (do-make-srcloc-token token srcloc)))

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
