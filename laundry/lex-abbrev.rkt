#lang racket/base

(require brag/support
         (for-syntax racket/base
                     #;
                     syntax/parse
                     (only-in racket/list combinations permutations)
                     ))
(provide (all-defined-out))

;; unused

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

#;
(define-lex-abbrev newlines (:+ (char-set "\xA\xD")))
#;
(define-lex-abbrev par-start (:or alpha 0-9 " " "\t"))
#;
(define-lex-abbrev par-rest-ok (:or par-start "-" "(" ")" "." "," "?" "!" "'" "\""
                                    ; "*" "/" "_" "+" "=" "~" ; markup markers can't be included
                                    "|" "\\" "^" ";"))
#;
(define-lex-abbrev paragraph (:+ (:seq "\n" par-start (:* (:or par-start par-rest-ok)) (:+ (:or par-start par-rest-ok)))))

;; atomic

(define-lex-abbrev 0-9 (char-set "0123456789"))
#; ; already defined by brag/support
(define-lex-abbrev lower-case (char-set "abcdefghijklmnopqrstuvwxyz"))
(define-lex-abbrev a-z lower-case)
#; ; already defined by brag/support
(define-lex-abbrev upper-case (char-set "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define-lex-abbrev A-Z upper-case)
#;
(define-lex-abbrev word (char-set "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define-lex-abbrev alpha (char-set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

#; ;
(define-lex-abbrev whitespace (:or "\n" "\t" " ")) ; pretty sure this is complete

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

;; elements

; fast heading detection since we can't parse them in one pass in a newline first grammar anyway
; note that this can't match a bof heading again due to the newline first design
; from/stop-before could also be used here, but negating the newline is probably ok
(define-lex-abbrev heading (:seq "\n" (:+ "*") (:or " " "\t") (:* (:~ "\n"))))

(define-lex-abbrev comment-element
  (:+ (:seq "\n" (:* " " "\t") "#" (:seq (:or " " "\t") (:* (:~ "\n"))))))

(define-lex-abbrev runtime-todo-keyword (:or "TODO" "DONE")) ; FIXME SIGH SIGH SIGH

;;; keywords

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

(define-lex-abbrev todo-spec-line ; XXX FIXME remove as overly complex
  ; XXX TODO FIXME BOF issues >_<
  (from/stop-before (:seq "\n"
                          (:* (:or " " "\t"))
                          "#+"
                          (:or "todo" "TODO")
                          ":"
                          " " ; XXX probably require this to avoid weirdness with colons inside keywords
                          )
                    "\n"))

;;; tables

(define-lex-abbrev table-element
  (from/stop-before (:+ (:seq "\n" (:* (:or " " "\t")) "|" (:* (:~ "\n"))))
                    (:or
                     "\n\n"
                     (:seq "\n"
                           (:* (:or " " "\t"))
                           (:~ (:or " " "\t" "\n" "|"))
                           (:* (:~ "\n"))
                           "\n"))))

;;; elements that cannot contain headings 

(define-lex-abbrev stop-before-heading (:seq "\n" (:+ "*") (:or " " "\t")))

;;;; blocks

(define-lex-abbrev src-block
  (:seq (from/stop-before (:seq "\n"
                                (:* " " "\t")
                                (:or "#+begin_src" "#+BEGIN_SRC")
                                (:* (:~ "\n")))
                          (:or
                           stop-before-heading
                           (:seq "\n" (:* " " "\t") (:or "#+end_src" "#+END_SRC") (:* " " "\t") "\n")))))

(module+ test-block
  (define src-lexer
    (lexer-srcloc
     [src-block (token 'SRC-BLOCK lexeme)]))
  (src-lexer (open-input-string "\n#+begin_src\n#+end_src\n#+end_src\n"))
  (src-lexer (open-input-string "\n#+begin_src\nin\n#+end_src\n#+end_src\n"))
  (src-lexer (open-input-string "
#+begin_src elisp
'lol
#+end_src
#+end_src"))

(define unk-lexer
    (lexer-srcloc
     [src-block (token 'SRC-BLOCK lexeme)]
     [unknown-block (token 'UNK-BLOCK lexeme)]
     ))

  (unk-lexer (open-input-string "\n#+begin_src\n#+end_src\n#+end_src\n"))
  (unk-lexer (open-input-string "\n#+begin_src\nin\n#+end_src\n#+end_src\n"))
  (unk-lexer (open-input-string "
#+begin_src elisp
'lol
#+end_src
#+end_src"))

  )

; block types named in the spec
; center
; quote
; comment
; example
; export
; src
; verse

(define-lex-abbrev unknown-block
  ; this parses to headings or to the next #+end_
  ; we cannot do the matching at this stage, so nested blocks will terminate
  ; early so we will have to detect mismatched suffixes in a second step
  ; OR we will have to terminate if we hit another #+begin_ block before
  ; finding an end, which might make more sense, but will have to test
  (:seq (from/stop-before (:seq "\n"
                                (:* " " "\t")
                                (:or (:seq "#+begin_"
                                           (:+ (:~ " " "\t" "\n"))
                                           (:or " " "\t")
                                           (:+ (:~ "\n")))
                                     (:seq "#+BEGIN_"
                                           (:+ (:~ " " "\t" "\n"))
                                           (:or " " "\t")
                                           (:+ (:~ "\n")))))
                          (:or
                           stop-before-heading
                           (:seq "\n"
                                 (:* " " "\t")
                                 (:or (:seq "#+end_"
                                            (:+ (:~ " " "\t" "\n"))
                                            (:? (:seq
                                                 (:or " " "\t")
                                                 (:* (:~ "\n")))))
                                      (:seq "#+END_"
                                            (:+ (:~ " " "\t" "\n"))
                                            (:? (:seq
                                                 (:or " " "\t")
                                                 (:* (:~ "\n"))))
                                            ))
                                 (:* " " "\t")
                                 "\n")))))

;;;; drawers

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

;; paragraphs

(define-lex-abbrev bullet-marker (:or "." ")"))

; parsing paragraph here in this way is an optimization that is absolutely necessary for performance
; in order to avoid some quadratic algorithm lurking somewhere in the grammar or in brag or in parser-tools
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
             (:* (:or " " "\t"))
             (:or
              (:or
               "'"
               "\""
               "("
               ")"
               "]"
               ","
               "!"
               "?"
               "%"
               "$"
               "^"
               "&"
               ".")
              (:seq (:or "+" "-") (:~ whitespace))
              #; ; broken ? or something else is wrong?
              (:seq (:+ "*") (:& (:~ whitespace) (:~ "*")))
              ; cases have to be paired to prevent (:~ "." ")") from further matching the same case
              ; FIXME TODO how to handle cases like \n123456789\n
              (:seq (:+ (:or " " "\t")) "[") ; footnote anchors and inline defs can start a paragraph line
              (:seq (:+ a-z) (:~ bullet-marker a-z "\n"))
              (:seq (:+ A-Z) (:~ bullet-marker A-Z "\n"))
              (:seq (:+ 0-9) (:~ bullet-marker 0-9 "\n"))
              (:seq
               (:or 
                (:+ lower-case)
                (:+ upper-case)
                (:+ 0-9))
               bullet-marker
               (:~ whitespace))))
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

;; paragraph parts

;;; citation

(define-lex-abbrev citation-key
  (:seq "@" (:+ (:or 0-9 A-Z a-z "-.:?!`'/*@+|(){}<>&_^$#%&~"))))

(define-lex-abbrev citation
  (from/to (:seq "[cite" (:* (:seq "/"
                                   ; FIXME I think this is wrong because it will match [cite//:] ?
                                   (:+ (:or 0-9 alpha "-" "_" "/"))))
                 ":"
                 (:* whitespace))
           ; TODO internal citation structure parser
           ; FIXME I think we need to stop before heading here as well?
           ; because [cite:\n* @key] would match this I think ...?
           (:seq "]")))

;;; footnote

;; footnotes are exceptionally tricky because they allow nesting other footnotes
;; so we can't actually use from/to for from/stop-before

(define-lex-abbrev footnote-label
  (:+ (:or 0-9 alpha "_" "-")))

(define-lex-abbrev footnote-anchor
  (:seq "[fn:" footnote-label "]")
  )

(define-lex-abbrev footnote-inline-start
  ; see footnote references
  ; the key constraint is that square brackets must be balanced
  ; which means that this portion cannot be handled in the tokenizer
  ; TODO technically these are allowed to contain only paragraph contents
  ; so not blocks or drawers
  (:seq "[fn:" (:? footnote-label) ":"))

(define-lex-abbrev footnote-inline-malformed ; FIXME -maybe-malformed, because it will match ok bits too but EOF causes issues
  ; FIXME verbatim issues probably

  ; FIXME arbitrary nesting cannot be handled effectively here

  ; FIXME this is incorrect, what we wanted was to detect cases where
  ; there was a double blank line (or was it single blank line?)
  ; before a RSB
  ; TODO this is going to need some special post processing
  ; yes footnotes are annoying to deal with due to need to match brackets
  ; we can't use from/stop-before for this because the gap will allow closing brackets to creep in
  (:seq footnote-inline-start (:* (:~ "]")) "\n\n")
  #;
  (from/stop-before (:seq footnote-inline-start
                          (:* (:~ "]")))
                    (:or "]"
                         "\n\n")))

(define-lex-abbrev footnote-inline-simple
  (:seq footnote-inline-start (:* (:~ "[" "]")) "]"))

(module+ test-fim
  (define fis-lexer
    (lexer-srcloc
     [footnote-inline-start (token 'FIS lexeme)]))

  (fis-lexer (open-input-string "[fn::Inline footnote[fn::Nested.].] more."))

  (define fisim-lexer
    (lexer-srcloc
     [footnote-inline-simple (token 'FISIM lexeme)]))

  (fisim-lexer (open-input-string "[fn::Inline footnote[fn::Nested.].] more."))

  (define fim-lexer
    (lexer-srcloc
     [footnote-inline-malformed (token 'FIM lexeme)]))

  ; expect to fail
  (fim-lexer (open-input-string "[fn::Inline footnote[fn::Nested.].] more."))
  )

(define-lex-abbrev stop-before-footnote-def
   (:seq "\n" "[fn:" footnote-label "]"))

(define-lex-abbrev footnote-definition
  ; FIXME "word constituent characters" not 100% sure what that means in this and other similar contexts
  (from/stop-before
   (:seq "\n" "[fn:" footnote-label "]")
   (:or
    stop-before-footnote-def
    stop-before-heading
    "\n\n\n"
    ; eof as well it seems
    )))

;;; hyperlink

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

(define-lex-abbrev hyperlink-ab ; XXX divergence
  ; FIXME XXX from a parsing perspective the current way that angle
  ; bracket links are defined has the same problem as todo keywords,
  ; they are runtime defined parts of the concrete syntax which causes
  ; all sorts of crazyness and inconsistency, what we do here instead
  ; is regularize this part of the grammar to remove any particular
  ; reference to protocol and make it fully generic, allowing the
  ; syntax to be entirely regular and making it possible to issue
  ; warnings when a protocol type is not known

  ; NOTE the protocol here is very close to a uri scheme, however I
  ; don't know what to do about the downcasing behavior
  (:seq "<"
        (:+ (:or a-z 0-9 "+" "." "-")) ":"
        (:+ (:~ (:or "]" "<" ">" "\n")))
        ">"))

;;; macro

;;; markup


(define-lex-abbrev mu-pre-not-newline-not-lcb (:or " " "\t" "-" "(" "'" "\""))
(define-lex-abbrev mu-pre-1 (:or "\n" mu-pre-not-newline-not-lcb "{"))
(define-lex-abbrev mu-pre (:+ mu-pre-1))
(define-lex-abbrev mu-pre-n (:>= 2 mu-pre-1))
(define-lex-abbrev mu-pre-n-not-lcb (:>= 2 mu-pre-not-newline-not-lcb "\n"))
; FIXME mu-post can also be newline, which is a problem due to newline
; first from/stop-before solves this
(define-lex-abbrev mu-post-not-newline (:or " " "\t" "-" "." "," ";" ":" "!" "?" "'" ")" "}" "[" "\""))
(define-lex-abbrev mu-post-1 (:or "\n" mu-post-not-newline))
(define-lex-abbrev mu-post (:+ mu-post-1))

(define-lex-abbrev mu-border (:~ whitespace))
(define-lex-abbrev mu-marker (:or "*" "/" "_" "+" "=" "~"))

; using from/stop-before and not worrying about the 3 line limit due to its increased complexity
; XXX the issue is that this eats any number of other things that have higher priority, such as
; headlines, so I'm 99% sure that this has to be done in a second pass that only applies to paragraphs

; FIXME TODO work out the rest of the patterns here
(define-lex-abbrev markup-= (:or (:seq (from/stop-before (:seq #;mu-pre "=" mu-border) (:seq mu-border "=" mu-post)) mu-border "=")
                                 (from/stop-before (:seq #;mu-pre "=" mu-border "=") mu-post)))
(define-lex-abbrev markup-~ (:or (:seq (from/stop-before (:seq #;mu-pre "~" mu-border) (:seq mu-border "~" mu-post)) mu-border "~")
                                 (from/stop-before (:seq #;mu-pre "~" mu-border "~") mu-post)))

(define-lex-abbrev markup-=-eof? (:or (:seq (from/stop-before (:seq #;mu-pre "=" mu-border) (:seq mu-border "=" #;mu-post)) mu-border "=")
                                      (:seq #;mu-pre "=" mu-border "=")))

(define-lex-abbrev markup-~-eof? (:or (:seq (from/stop-before (:seq #;mu-pre "~" mu-border) (:seq mu-border "~" #;mu-post)) mu-border "~")
                                      (:seq #;mu-pre "~" mu-border "~")))

; FIXME TODO investigate interactions between verbatim/code and other markup XXXXXXXXXX
; technically org immplements this by applying the paragraph parser repeatedly to the
; internal contents of markup, which is ... probably less than optimal to say the least

(define-syntax (make-markup-abbrevs stx)
  (let ([combs (map (λ (chars) (apply string chars))
                    (cdr (combinations (string->list "*/_+"))))])
    #`(begin #,@
        (for/list ([c combs])
          (list 'begin
                (list 'provide (string->symbol (format "markup-~a" c)))
                (list 'define-lex-abbrev
                      (string->symbol (format "markup-~a" c))
                      (cons ':or
                            (for/list ([mut (map (λ (chars) (apply string chars))
                                                 (permutations (string->list c)))])
                              (define stop (list->string (reverse (string->list mut))))
                              ; TODO check behavior to make sure cases like =* a b * c d e *= work
                              #`(:or (:seq (from/stop-before (:seq #;mu-pre #,mut mu-border)
                                                             (:seq mu-border #,stop mu-post))
                                           mu-border
                                           #,stop)
                                     (from/stop-before (:seq #;mu-pre #,mut mu-border #,stop) mu-post)))))
                (list 'provide (string->symbol (format "markup-~a-eof?" c)))
                (list 'define-lex-abbrev
                      (string->symbol (format "markup-~a-eof?" c))
                      (cons ':or
                            (for/list ([mut (map (λ (chars) (apply string chars))
                                                 (permutations (string->list c)))])
                              (define stop (list->string (reverse (string->list mut))))
                              ; TODO check behavior to make sure cases like =* a b * c d e *= work
                              #`(:or (:seq (from/stop-before (:seq #;mu-pre #,mut mu-border)
                                                             (:seq mu-border #,stop #;(:? mu-post)))
                                           mu-border
                                           #,stop)
                                     (:seq #;mu-pre #,mut mu-border #,stop))))))))))

(make-markup-abbrevs)

;; other parser bits

(define-lex-abbrev section (from/stop-before heading heading)) ; FIXME BOF EOF issues
