#lang racket/base
(require racket/pretty)
(require brag/support
         "lex-help.rkt"
         (only-in racket/string string-suffix?)
         (only-in racket/port input-port-append)
         (for-syntax racket/base
                     #;
                     syntax/parse
                     (only-in racket/list combinations permutations)
                     ))
(provide laundry-make-tokenizer
         paragraph-make-tokenizer
         ;heading-make-tokenizer
         bind-runtime-todo-keywords

         heading
         hyperlink
         comment-element
         drawer-ish

         paragraph
         #|
         markup-*
         markup-/
         markup-_
         markup-+
         markup-=
         markup-~
         |#
         )
(define-lex-abbrev newlines (:+ (char-set "\xA\xD")))
(define-lex-abbrev 0-9 (char-set "0123456789"))
(define-lex-abbrev lower-case (char-set "abcdefghijklmnopqrstuvwxyz"))
(define-lex-abbrev upper-case (char-set "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
#;
(define-lex-abbrev word (char-set "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define-lex-abbrev alpha (char-set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

; fast heading detection since we can't parse them in one pass in a newline first grammar anyway
; note that this can't match a bof heading again due to the newline first design
; from/stop-before could also be used here, but negating the newline is probably ok
(define-lex-abbrev heading (:seq "\n" (:+ "*") (:or " " "\t") (:* (:~ "\n"))))

; XXX this is better than the from/to version in one sense, but worse
; in another becuase this one is greedy
; XXX this is also incorrect due to the fact that hyperlinks support
; backslash escape
; https://orgmode.org/list/87tvguyohn.fsf@nicolasgoaziou.fr/T/#u
; https://orgmode.org/list/87sgvusl43.fsf@nicolasgoaziou.fr/T/#u
(define-lex-abbrev hyperlink-content
  (:+ (:or (:~ "[" "]" "\n") (:seq "\\" "[")  (:seq "\\" "]"))))
; FIXME not sure if correct
(define-lex-abbrev hyperlink
  (:seq "[[" hyperlink-content (:? (:seq "][" hyperlink-content) ) "]]"))

(define-lex-abbrev comment-element
  (:+ (:seq "\n" (:* " " "\t") "#" (:seq (:or " " "\t") (:* (:~ "\n"))))))

(define-lex-abbrev keyword-element
  (from/stop-before
   (:seq "\n"
         (:* " " "\t")
         "#+"
         (:or
          (:seq
           (:* (:~ whitespace "["))
           "["
           (:* (:~ "\n"))
           "]")
          (:* (:~ whitespace)))
         ":"
         ; XXX BUT there must be a space before the first colon can appear in the value
         ; otherwise the :~ whitespace will continue to match, but I think it will do that
         ; anyway? so we are ok?
         (:* (:~ "\n")))
   "\n"))
(define-lex-abbrev src-block
  (:seq (from/stop-before (:seq "\n" (:* " " "\t") (:or "#+begin_src" "#+BEGIN_SRC"))
                          (:or
                           stop-before-heading
                           (:seq "\n" (:* " " "\t") (:or "#+end_src" "#+END_SRC") (:* " " "\t") "\n")))))

(define-lex-abbrev table-element
  (from/stop-before (:+ (:seq "\n" (:* (:or " " "\t")) "|" (:+ (:~ "\n"))))
                    (:or
                     "\n\n"
                     (:seq "\n" (:* (:or " " "\t")) (:~ " " "\t" "\n" "|") (:+ (:~ "\n")) "\n"))))
(define-lex-abbrev stop-before-heading (:seq "\n" (:+ "*") (:or " " "\t")))
(define-lex-abbrev drawer-props
  (:or (from/stop-before (:seq "\n" (:* " " "\t") ":properties:")
                         ; FIXME NOTE there are cases where the pattern in the
                         ; names of the property values for a draw cause it to NO
                         ; LONGER BE a property drawer, this is almost certainly a
                         ; design flaw in org mode which makes it difficult to
                         ; parse the property drawers correctly and efficiently in
                         ; this way, the other possibility is to accept the more
                         ; complex grammar that we already wrote which more or
                         ; less works as expected at the expense of performance
                         (:or stop-before-heading
                              (:seq "\n" (:* " " "\t") ":end:" (:* " " "\t") "\n")))
       ; sigh legacy support for this
       (from/stop-before (:seq "\n" (:* " " "\t") ":PROPERTIES:") ; FIXME does the case have to match?
                         (:or stop-before-heading
                              (:seq "\n" (:* " " "\t") ":END:" (:* " " "\t") "\n")))))

(define-lex-abbrev drawer-ish
  ; FIXME this is not right, check the spec to see
  (from/stop-before
   (:seq "\n" (:* " " "\t") ":" (:+ (:or 0-9 alpha "-" "_")) ":")
   (:or stop-before-heading
        (:seq "\n" (:* " " "\t") ":end:" (:* " " "\t") "\n"))))

(define-lex-abbrev todo-spec-line
  ; XXX TODO FIXME BOF issues >_<
  (from/stop-before (:seq "\n"
                          (:* (:or " " "\t"))
                          "#+"
                          (:or "todo" "TODO")
                          ":"
                          " " ; XXX probably require this to avoid weirdness with colons inside keywords
                          )
                    "\n"))

(define-lex-abbrev par-start (:or alpha 0-9 " " "\t"))
(define-lex-abbrev par-rest-ok (:or par-start "-" "(" ")" "." "," "?" "!" "'" "\""
                                    ; "*" "/" "_" "+" "=" "~" ; markup markers can't be included
                                    "|" "\\" "^" ";"))
; parsing paragraph here in this way is an optimization that is absolutely necessary for performance
; in order to avoid some quadratic algorithm lurking somewhere in the grammar or in brag or in parser-tools
#;
(define-lex-abbrev paragraph (:+ (:seq "\n" par-start (:* (:or par-start par-rest-ok)) (:+ (:or par-start par-rest-ok)))))
(define-lex-abbrev paragraph
  ; things we cannot start with
  ; *+[ \t]
  ; [ \t]+[+-*][ \t]
  ; [ \t]+[A-Za-z0-9][.)]
  ; |
  ; #[ \t]
  ; #\+
  ; :[A-Za-z]+:
  (:+ (from/stop-before
       (:seq "\n"
             ;(:~ "*" "")
             (:* " " "\t")
             (:or
              (:or
               "'"
               "\""
               "("
               ")"
               ","
               "!"
               "?"
               "%"
               "$"
               "^"
               "&"
               ".")
              (:seq (:or "+" "-" "*") (:~ whitespace))
              (:seq
               (:or 
                (:+ lower-case)
                (:+ upper-case)
                (:+ 0-9))
               (:or (:~ "." ")")
                    (:seq (:or "." ")") (:~ whitespace))))))
       "\n"))
  #;
  (:+ (:seq "\n" (:seq (:* " " "\t")
                       (:+ (:or
                            lower-case
                            upper-case
                            0-9
                             ))
                       (:& (:~ "." ")") par-rest-ok)
                       (:+ par-rest-ok)))))


(define-lex-abbrev negated-set
  (:~ ; NOT any of these
   " "
   "\t"
   "\n"

   0-9
   alpha
   "\\"
   "\""

   "|"
   ":"
   "%"
   "_"

   "*"
   "@"
   "+"
   "-"
   "["
   "]"
   ">"
   ))

;(define-lex-abbrev month-major-digit (char-set "01"))
;(define-lex-abbrev day-major-digit (char-set "0123"))
;(define-lex-abbrev time-major-digit (char-set "012"))

#;
(define-syntax (dla stx)
  (syntax-parse stx
    ([_ name list-of-strings]
     #`(define-lex-abbrev name (or #,@())))))

#;
(begin-for-syntax
  (define-struct lex-abbrev (get-abbrev))) ; will this well be a doozy

(define (find-last char str)
  ; we don't want to do string reverse because it is slow we just want
  ; to find the first char going backward, string-ref backward seems
  ; like it is ok if we really want this to go fast maybe could chunk
  ; bytes into cacheline size and search for a sub-offset within that?
  (letrec ([search (λ (look-at)
                     (println (string-ref str look-at))
                     (if (eq? (string-ref str look-at) char)
                         (values (substring str 0 look-at) (sub1 look-at))
                         (search (sub1 look-at))))])
    (search (sub1 (string-length str)))))

(define (token-stop-before TOKEN TOKEN-EOF lexeme char input-port start-pos)
  "Encapsulates the machinery needed to correctly reset the location of input-port when
using from/stop-before where the stop-before pattern contains multiple charachters."
  (if (eq? (peek-char input-port) eof)
      (token TOKEN-EOF lexeme)
      (let*-values ([(lexeme-correct offset) (find-last char lexeme)]
                    [(token-correct position-correct)
                     (values (token TOKEN lexeme-correct)
                             (+ (position-offset start-pos) offset))])
        (if (or (file-stream-port? input-port)
                (string-port? input-port)) ; cooperate with laundry lexer first-port
            (begin
              (file-position input-port position-correct)
              token-correct)
            ; FIXME we will have to come up with a better solution where we can set the
            ; file position on the appended ports for the first token so that everything
            ; will compose correctly for chaining input ports
            (cons token-correct position-correct)))))

(define (token-stop-before-heading TOKEN lexeme input-port start-pos)
  (if (string-suffix? lexeme "*")
      (let ([TOKEN-MALFORMED (string->symbol (format "~a-MALFORMED" TOKEN))])
        (token-stop-before TOKEN-MALFORMED TOKEN-MALFORMED lexeme #\newline input-port start-pos))
      (if (eq? (peek-char input-port) eof)
          ; FIXME decide how we are goin to do this and make it consistent
          (token (string->symbol (format "~a-EOF" TOKEN)) lexeme)
          (token TOKEN lexeme))))

(define (get-tokens tokenizer)
  (define (sigh next-token [accum '()])
    (let ([t (next-token)])
      (if (eq? t eof)
          accum
          (sigh next-token (cons t accum)))))
  (reverse (sigh tokenizer)))

(define-lex-abbrev runtime-todo-keyword (:or "TODO" "DONE")) ; FIXME SIGH SIGH SIGH

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
              (:+ (:seq ":" (:+ (:or alpha 0-9 "_" "@" "#" "%"))))
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
       [runtime-todo-keyword (token 'RUNTIME-TODO-KEYWORD lexeme)]
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
       [any-char #;(:~ "*") (token 'OOPS lexeme)]
       ))
  (define (heading-make-tokenizer port)
    ; FIXME can't run compile-syntax due to use of eval above probably?
    (define heading-lexer (eval-syntax heading-lexer-src))
    (define (next-token)
      (let ([out (heading-lexer port)])
        #;
        (pretty-print out)
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

(define paragraph-lexer
  (lexer-srcloc
   [(from/to (:seq "{{{" alpha) "}}}") (token 'MACRO lexeme)]
   #; ; the spec for macro reads like it can be multi-line
   [(:seq "{{{" alpha (:+ (:or alpha 0-9 "-" "_")) (:? (:seq "(" ")")) "}}}") (token 'MACRO lexeme)]
   [(:+ (:~ mu-pre-1 mu-marker mu-post-1)) (token 'STUFF-B lexeme)]

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

   [markup-* (token 'BOLD lexeme)]
   [markup-/ (token 'ITALIC lexeme)]
   [markup-_ (token 'UNDERLINE lexeme)]
   [markup-+ (token 'STRIKE lexeme)]

   [markup-= (token 'VERBATIM lexeme)]
   [markup-~ (token 'CODE lexeme)]

   [markup-*-eof? (token 'BOLD-EOF lexeme)]
   [markup-/-eof? (token 'ITALIC-EOF lexeme)]
   [markup-_-eof? (token 'UNDERLINE-EOF lexeme)]
   [markup-+-eof? (token 'STRIKE-EOF lexeme)]

   [markup-=-eof? (token 'VERBATIM-EOF lexeme)]
   [markup-~-eof? (token 'CODE-EOF lexeme)]

   #; ; busted
   [(:+ (:or mu-pre mu-post)) (token 'STUFF-A lexeme)]

   [(:+ (:or (:& (:~ mu-pre-1) mu-post-1) mu-marker)) (token 'STUFF-A lexeme)]

   [mu-pre-n-not-lcb (token 'MU-PRE-N-NOT-LCB lexeme)] ; LCB cannot be in mu-pre-n because it would block macro
   [mu-pre-1 (token 'MU-PRE-1 lexeme)] ; needed to prevent accidental capture when :+ length for stuff is longer than markup

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

(define (paragraph-make-tokenizer port)
  (define (next-token)
    (let ([out (paragraph-lexer port)])
      #;
      (pretty-print out)
      out))
  next-token)

;; I have no idea why this was defined inside of make-tokenizer in the
;; original example I copied years ago ...

;; ah, now I see, sometimes you want/need to be able to configure the
;; lexer at runtime, so for exaple org reconfigures startup options
;; searching out the initial configuration of various #+todo: keywords
;; which may appear anywhere in the file (oops) so a full simple parse
;; would have to be conducted to find them first, OR we specify that
;; #+todo: keywords defined outside the first section of the org-file
;; are not guranteed to be included during any given parse

;; XXX FALSE, there is no easy way to define lexer-srcloc
;; why the foo do people define these things as macros ?!?!?!?!
;; there is no reason to force it to be so fooing rigid
;; all in the name of safety I'm sure, so annoying though
;; why the hell can't I just pass the data in!

;; XXX further fooery: ah yes, even more problems which is
;; that you can't compile the fooing lexer so that it is
;; performant at runtime when you need it because this thing
;; wait no ... this can't be the case ... because the macro
;; used to be invoked only inside a function call ?
;; ah foo, but it could still be compiled ahead of time

(define laundry-lexer
  (lexer-srcloc

   ; for testing secondary headline parser
   #;
   ["TODO" (token 'RUNTIME-TODO-KEYWORD lexeme)]
   #;
   ["DONE" (token 'RUNTIME-TODO-KEYWORD lexeme)]

   [heading (token 'HEADING lexeme)]

   ; FIXME there is a question of whether to use the lexer like this
   ; to slurp code blocks in the first pass and then run code block
   ; specific parser in a later pass, or whether we want to try to
   ; get certain information in the first pass
   #; ; XXX doesn't work the match is too short
   [(from/stop-before (:seq "\n" (:* " " "\t") "#+begin_src")
                      (:seq "\n" (:+ "*") (:or " " "\t")))
    (token 'SRC-BLOCK-BEGIN-MALFORMED lexeme)]
   ; XXX we can't do start-after on a heading to find a random end_src
   [src-block (token-stop-before-heading 'SRC-BLOCK lexeme input-port start-pos)]

   [table-element (token 'TABLE-ELEMENT lexeme)] ; FIXME stop before corrections
   [keyword-element (token 'KEYWORD-ELEMENT lexeme)] ; before hyperlink for #+[[[]]]:asdf
   [hyperlink (token 'LINK lexeme)] ; as it turns out this also helps performance immensely
   ; in theory it should be possible to scan for headlines and then parse all the sections
   ; in parallel

   [comment-element (token 'COMMENT-ELEMENT lexeme)]

   [drawer-props (token-stop-before-heading 'DRAWER-PROPS lexeme input-port start-pos)]
   [drawer-ish (token-stop-before-heading 'DRAWER lexeme input-port start-pos)]

   [paragraph (token 'PARAGRAPH lexeme)] ; needed for performance reasons to mitigate quadratic behavior around short tokens


   [(:>= 2 "*") (token 'STARS lexeme)] ; need this in lexer otherwise performance tanks in the parser
   ["*" (token 'ASTERISK lexeme)]
   ["\n" (token 'NEWLINE)]
   [" " (token 'SPACE)]
   #; ; TODO should help with perf
   [(:+ " ") (token 'SPACE-N lexeme)]
   ["\t" (token 'TAB)]

   ; these are lexemes so that we can ignore them if they don't start the line

   ["[fn:" (token 'FOOTNOTE-START lexeme)]
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

   [(:or ":PROPERTIES:" ":properties:") (token 'PROPERTIES-D lexeme)] ; sigh
   [(:or ":END:" ":end:") (token 'END-D lexeme)] ; sigh

   ; FIXME yeah, this is is a pain
   [(:or "#+begin_src" "#+BEGIN_SRC") (token 'BEGIN-SRC lexeme)]
   [(:or "#+end_src" "#+END_SRC") (token 'END-SRC lexeme)]
   [(:or "#+begin_example" "#+BEGIN_EXAMPLE") (token 'BEGIN-EX lexeme)]
   [(:or "#+end_example" "#+END_EXAMPLE") (token 'END-EX lexeme)]

   [(:or "#+begin_" "#+BEGIN_") (token 'BEGIN-BLOCK lexeme)]
   [(:or "#+end_" "#+END_") (token 'END-BLOCK lexeme)]

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
   [(:or "#+CALL" "#+call") (token 'CALL lexeme)]

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
   ["." (token 'PERIOD lexeme)]
   [":" (token 'COLON lexeme)]
   ["[" (token 'LSB lexeme)]
   ["]" (token 'RSB lexeme)]

   ["_" (token 'UNDERSCORE lexeme)]
   ["-" (token 'HYPHEN lexeme)]
   ["+" (token 'PLUS lexeme)]
   ["#" (token 'HASH lexeme)]
   ["@" (token 'AT lexeme)]
   ["%" (token 'PERCENT lexeme)]

   ["|" (token 'PIPE lexeme)]
   ["<" (token 'LAB lexeme)]
   [">" (token 'RAB lexeme)]
   ["{" (token 'LCB lexeme)]
   ["}" (token 'RCB lexeme)]
   ["'" (token 'SQ lexeme)]
   ["\"" (token 'DQ lexeme)]
   ["\\" (token 'BS lexeme)]
   
   ; strings don't work because longest match means we can never detect the escape sequence
   ;[(from/to "\"" "\\") (token 'DQ-TO-ESC lexeme)]
   ;[(from/to "\"" "\"") (token 'DQ-TO-DQ lexeme)] ; ARGH longest match kills us here again :/

   ["(" (token 'L-PAREN lexeme)]
   [")" (token 'R-PAREN lexeme)]

   [0-9 (token 'DIGIT lexeme)]
   [(:= 2 0-9) (token 'DIGIT-2 lexeme)] ; dates
   [(:= 3 0-9) (token 'DIGIT-3 lexeme)] ; dates
   [(:= 4 0-9) (token 'DIGIT-4 lexeme)] ; dates
   [(:>= 5 0-9) (token 'DIGIT-N lexeme)]
   ;["l" (token 'CHAR-LOWER-L lexeme)] ; HAH BEGONE FOUL INSTANCE OF A THING
   ["X" (token 'CHAR-UPPER-X lexeme)] ; FOO
   [alpha (token 'ALPHA lexeme)] ; FIXME TODO alpha+ ?
   ;[(:>= 2 alpha) (token 'ALPHA-N lexeme)] ; XXX longest match kills this for COMMENT and ARCHIVE
   [(:>= 2 lower-case) (token 'ALPHA-LOWER-N lexeme)]
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
    (if bof ; sigh branches instead of rewriting
        (let ([first-port (input-port-append #f (open-input-string "\n") port)])
          ; this works because the position of the original port is
          ; correctly shifted so its coordinates should not be disturbed
          (let* ([-first-out (laundry-lexer first-port)]
                 [shift (- (file-position first-port) (file-position port))]
                 [first-out (if (list? -first-out)
                                (begin
                                  ; FIXME this is a hack, we need to have a variant of
                                  ; input-port-append that seeks the second stream
                                  (file-position port (- (cdr -first-out) shift))
                                  (car -first-out))
                                -first-out)]
                 [out (if (srcloc-token? first-out)
                          (make-srcloc-token
                           (let* ([t (srcloc-token-token first-out)]
                                  #;
                                  [__ (println (cons 'qq t))]
                                  [v (token-struct-val t)])
                             ; FIXME must strip the leading newline
                             (token
                              (token-struct-type t)
                              (if (string? v)
                                  (substring v 1)
                                  v)))
                           (let* ([sl (srcloc-token-srcloc first-out)]
                                  [line (srcloc-line sl)]
                                  [srcloc-correct
                                   (make-srcloc
                                    (srcloc-source sl)
                                    (if line (sub1 line) line) ; FIXME careful with -> 0
                                    (srcloc-column sl)
                                    (srcloc-position sl) ; should always be 1
                                    (- (srcloc-span sl) shift))])
                             srcloc-correct))
                          first-out)])
            (set! bof #f)
            #;
            (pretty-print out)
            out))
        (let* ([out-raw (laundry-lexer port)]
               #;
               [__ (println (cons 'aaaaaaaaaaa out-raw))]
               [out
                (if (and (not (eq? out-raw eof))
                         (not (let ([t (srcloc-token-token out-raw)])
                                #;
                                (println (cons 'asdf t))
                                (or (eq? (token-struct-type t) 'NEWLINE)
                                    (string-suffix?
                                     (token-struct-val t)
                                     "\n"))))
                         (eq? (peek-char port) eof)) ; TODO and lexeme end != newline
                    (begin
                      ; FIXME we know the length of these files so there is no reason to peek?! these
                      ; files are not things that should be appended to while we are reading them and
                      ; while I guess you could have an org file that wouldn't fit into memory that's
                      ; the point at which a ... different approach will be needed anyway ... actually
                      ; you're going to have to branch on something at some point
                      (file-position port
                                     (- (file-position port) ; XXX check this
                                        (srcloc-span (srcloc-token-srcloc out-raw))))
                      (let ([final-out
                             (laundry-lexer
                              (input-port-append #f
                                                 port
                                                 (open-input-string "\n")))])
                        #;
                        (pretty-print final-out)
                        final-out))
                    out-raw)])
          #;
          (pretty-print out)
          out)))
  next-token)

(module+ test-port
  (define port (open-input-string "sigh\nhello\n* there"))
  (define prefix-port (open-input-string "\n"))
  (define first-time-port (input-port-append #f prefix-port prefix-port port))
  (define tport (open-input-string "* Hello world\nand now for"))
  (define ll (laundry-make-tokenizer #;make-laundry-lexer tport))
  )

(define-lex-abbrev section (from/stop-before heading heading)) ; FIXME BOF EOF issues

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
