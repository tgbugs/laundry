#lang racket/base

(require brag/support
         (for-syntax racket/base
                     ;#;
                     syntax/parse
                     (only-in racket/syntax format-id)
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

(define-lex-abbrev wsnn (:& whitespace (:~ "\n")))
(define-lex-abbrev wsnn* (:* wsnn))
(define-lex-abbrev wsnn+ (:+ wsnn))

(define-lex-abbrev negated-set ; aka NOT stop words
  (:~ ; NOT any of these
   " " ; only here to differentiate whitespace
   "\t"
   "\n"

   0-9
   alpha
   ;"\\" ; this is eol in paragraphs and no longer an issue
   ;"\"" ; this is sandboxed to headings and is no longer an issue

   "|" ; XXX maybe we can remove this
   ":" ; XXX maybe we can remove this
   ;"%" ; this is sandboxed in timestamps or paragraphs and is no longer and issue
   ;"_" ; XXX sandboxed in ??? but definitely not an issue to star with it

   "*"
   ;"@" ; this is sandboxed in paragraphs and no longer an issue
   "+"
   "-"
   "["
   "]" ; XXX maybe we can remove this
   ;">" ; FIXME why was this here at all?
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

(define-lex-abbrev keyword-element ; FIXME broken for #+begin_: I think but also consider that maybe keyword should take precendence in this case?
  (from/stop-before
   (:seq "\n"
         (:* " " "\t")
         "#+"
         (:or
          (:* (:~ whitespace))
          (:seq
           (:* (:~ whitespace "["))
           "["
           (:* (:~ "\n"))
           "]"))
         ":"
         ; XXX BUT there must be a space before the first colon can appear in the value
         ; otherwise the :~ whitespace will continue to match, but I think it will do that
         ; anyway? so we are ok?
         (:* (:~ "\n")))
   "\n"))

(define-lex-abbrev keyword-element-whitespace
  (:& (:seq (:? (:~ "\n")) wsnn+ (:? (:~ "\n")))
      (:* (:or whitespace (:~ (:or "[" "]" "\n"))))))

(define-lex-abbrev keyword-post-colon
  (:* (:~ (:or ":" "\n"))))

#;
(define-lex-abbrev keyword-not-begin
  (:& (:+ (:~ (:or ":" "\n")))
      (:or
       (:~ "b")
       (:seq "b" (:~ "e"))
       (:seq "be" (:~ "g"))
       (:seq "beg" (:~ "i"))
       (:seq "begi" (:~ "n"))
       (:seq "begin" (:~ "_"))
       #; ; match as malformed block
       (:seq "begin_" wsnn+)
       #; ; match as malformed block
       (:& (:seq "begin_")
           (from/stop-before "b" "\n"))
       (:~ "B")
       (:seq "B" (:~ "E"))
       (:seq "BE" (:~ "G"))
       (:seq "BEG" (:~ "I"))
       (:seq "BEGI" (:~ "N"))
       (:seq "BEGIN" (:~ "_"))
       #; ; match as malformed block
       (:seq "BEGIN_" wsnn+)
       #; ; match as malformed block
       (:& (:seq "BEGIN_")
           (from/stop-before "B" "\n")))))

(define-lex-abbrev keyword-element-malformed
  ; FIXME this doesn't cover with cases of malformed options
  (:& (from/stop-before "\n" "\n") ; stop before isn't actually necessary if there is a shorter match ...
      (complement
       (:seq
        "\n"
        (:* " " "\t")
        "#+"
        (:or "begin" "BEGIN" "end" "END")
        "_"
        (:~ (:or " " "\t"))
        any-string))
      (:seq
       "\n"
       (:* " " "\t")
       "#+"
       ; the malformed options cases ??
       ; [ whitespace missing ] :
       ; not [ whitespace not ] :
       ; whitespace ] anything :
       ; basically whitespace not between [ and ]
       ; or whitespace between [ and ] but something after ] that is not : and then no colon for reset of line
       ; FIXME dealing with #+begin_ case is a pain here because we have to enumerate
       (:?
        ;(:&
         #; ; using complement above covers all the cases here
         keyword-not-begin
         (:or
          (:seq
           (:* (:~ (:or "\n"))) ; FIXME the not colon case ? or does keyword-post-colon save us here?
           wsnn+ ; any whitespace
           (:* (:~ (:or "\n")))
           (:~ (:or "]")) ; and the char before the colon is not rsb
           ":")
          (:seq
           (:* (:~ ":" whitespace))
           wsnn+
           ":"
           )
          (:seq
           (:* (:~ (:or "\n" "[")))
           wsnn+ ; any whitespace before the first lsb
           (:* (:~ (:or "\n"))) ; and rsb before colon
           "]:")
          #;
          (:?
           (:seq
            (:+ (:~ (:or "b" "B" ":" "\n"))) ; #+begin_src etc.
            )))
         ;)
       )
       keyword-post-colon)))

(module+ test-keyword-malformed
  (define nk-lexer
    (lexer-srcloc
     [keyword-element (token 'KEYWORD-ELEMENT lexeme)] ; have to have this for longer match
     [keyword-element-malformed (token 'KEYWORD-ELEMENT-MALFORMED lexeme)]))

  (nk-lexer (open-input-string "\n#+ :\n"))
  (nk-lexer (open-input-string "\n#+x :\n"))

  (nk-lexer (open-input-string "\n#+]]]:\n"))
  (nk-lexer (open-input-string "\n#+a b: \n"))
  (nk-lexer (open-input-string "\n#+a b:\n"))

  (nk-lexer (open-input-string "\n#+call:\n"))

  (nk-lexer (open-input-string "\n#+call: hello\n"))

  (nk-lexer (open-input-string "\n#+[hello]: lol\n"))
  (nk-lexer (open-input-string "\n#+[a b]: lol\n"))
  (nk-lexer (open-input-string "\n#+[a b]x: lol\n"))


  (nk-lexer (open-input-string "\n#+lolol hello world\n"))

  (nk-lexer (open-input-string "\n#+b\n"))
  (nk-lexer (open-input-string "\n#+begin\n"))
  (nk-lexer (open-input-string "\n#+beginx\n"))
  ; these are matched as malformed blocks ? FIXME but somehow they match here too ???
  (nk-lexer (open-input-string "\n#+begin_\n"))
  (nk-lexer (open-input-string "\n#+begin_ \n"))

  ; this matches a shorter string and the block parser takes over
  (nk-lexer (open-input-string "\n#+begin_src\n"))

  )

#;
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
  (:+ (from/stop-before (:seq "\n" wsnn* "|") "\n"))
  #;
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

(define-lex-abbrev src-block-line-begin
  (:seq "\n"
        (:* " " "\t")
        (:or "#+begin_src" "#+BEGIN_SRC")
        (:* (:~ "\n"))))

(define-lex-abbrev src-block-line-end
  (:seq "\n" (:* " " "\t") (:or "#+end_src" "#+END_SRC") (:* " " "\t")))

(define-lex-abbrev src-block
  (:seq (from/stop-before
         src-block-line-begin
         (:or
          stop-before-heading
          (:seq src-block-line-end "\n")))))

(module+ test-block
  (define src-lexer
    (lexer-srcloc
     [src-block (token 'SRC-BLOCK lexeme)]))
  (src-lexer (open-input-string "\n#+begin_src\n#+end_src\n#+end_src\n"))
  (src-lexer (open-input-string "\n#+begin_src\nin\n#+end_src\n#+end_src\n"))

  (src-lexer (open-input-string "
#+begin_src
a
#+end_src
b
#+begin_src
c
#+end_src
"))

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

  (unk-lexer (open-input-string "
#+begin_h
a
#+end_h
b
#+begin_h
c
#+end_h
"))

  (define us-lexer
    (lexer-srcloc
     [(from/stop-before unknown-block-line-begin "\n")
      lexeme]
     ))

  (us-lexer (open-input-string "
#+begin_h
#+end_h
"))


  )

; block types named in the spec
; center
; quote
; comment
; example
; export
; src
; verse

(define-lex-abbrev unknown-block-line-begin
  (:seq "\n"
        (:* " " "\t")
        (:or (:seq "#+begin_"
                   (:+ (:~ " " "\t" "\n"))
                   (:?
                    (:seq
                     (:or " " "\t")
                     (:+ (:~ "\n")))))
             (:seq "#+BEGIN_"
                   (:+ (:~ " " "\t" "\n"))
                   (:?
                    (:seq
                     (:or " " "\t")
                     (:+ (:~ "\n"))))))))

(define-lex-abbrev unknown-block-line-end
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
        (:* " " "\t")))

(define-lex-abbrev unknown-block
  ; this parses to headings or to the next #+end_
  ; we cannot do the matching at this stage, so nested blocks will terminate
  ; early so we will have to detect mismatched suffixes in a second step
  ; OR we will have to terminate if we hit another #+begin_ block before
  ; finding an end, which might make more sense, but will have to test
  (:& (:seq any-string unknown-block-line-end)
   (:seq (from/stop-before unknown-block-line-begin
                           (:or
                            stop-before-heading
                            (:seq unknown-block-line-end "\n"))))))

;;;; drawers

(define-lex-abbrev drawer-end-comp
  (complement (:seq any-string (:or ":end:" ":END:") (:* " " "\t") (:+ (:~ (:or " " "\t" "\n"))))))

(define-lex-abbrev drawer-prop-comp
  (complement (:seq "\n" (:* " " "\t") ":properties:" (:* " " "\t") (:+ (:~ (:or " " "\t" "\n"))))))

(define-lex-abbrev drawer-PROP-comp
  (complement (:seq "\n" (:* " " "\t") ":PROPERTIES:" (:* " " "\t") (:+ (:~ (:or " " "\t" "\n"))))))

(define-lex-abbrev drawer-props
  (:or (:& (from/stop-before
            (:seq "\n" (:* " " "\t") ":properties:" (:* " " "\t"))
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
           ; I don't understand how the variant above matches things where :end: has 
           ; something other than whitespace in front of it :/
           drawer-prop-comp
           (:or #; (:seq any-string "\n" (:+ "*"))
                (:seq any-string "\n" (:* " " "\t") ":end:" (:* " " "\t"))))
       ; sigh legacy support for this
       (:& (from/stop-before
            (:seq "\n" (:* " " "\t") ":PROPERTIES:" (:* " " "\t"))
            (:or stop-before-heading
                 (:seq "\n" (:* " " "\t") ":END:" (:* " " "\t") "\n")))
           drawer-PROP-comp
           (:or #; (:seq any-string "\n" (:+ "*"))
                (:seq any-string "\n" (:* " " "\t") ":END:" (:* " " "\t"))))
       (:& drawer-end-comp
        (from/stop-before
         (:or
          (:seq "\n" (:* " " "\t") ":properties:" (:* " " "\t") "\n" (:* " " "\t") ":end:" (:* " " "\t"))
          (:seq "\n" (:* " " "\t") ":PROPERTIES:" (:* " " "\t") "\n" (:* " " "\t") ":END:" (:* " " "\t")))
         "\n"))))

(define-lex-abbrev drawer-start-line
  (:& (complement (:seq "\n" (:* " " "\t") (:or ":end:" ":END:") (:* " " "\t")))
      (:seq "\n" (:* " " "\t") ":" (:+ (:or 0-9 alpha "-" "_")) ":" (:* " " "\t"))))

(define-lex-abbrev drawer-start
  (:seq drawer-start-line "\n"))

(define-lex-abbrev drawer-end-line
  (:seq "\n" (:* " " "\t") ":end:" (:* " " "\t")))

(define-lex-abbrev drawer-end
  (:seq drawer-end-line "\n"))

(define-lex-abbrev drawer-ish-comp-begin
  ; FIXME seq this any-string?
  (complement (:seq "\n" (:* " " "\t") ":" (:+ (:or 0-9 alpha "-" "_")) ":" (:* " " "\t")
                    (:+ (:~ (:or " " "\t" "\n"))))))

(define-lex-abbrev drawer-ish-comp-end
  (:or #; (:seq any-string "\n" (:+ "*")) ; FIXME what happens if we leave this out ? TODO apparently it correctly does not match as a drawer?
       (:seq any-string "\n" (:* " " "\t") (:or ":end:" ":END:") (:* " " "\t"))))

(define-lex-abbrev drawer-ish
  ; FIXME this is not right, check the spec to see
  (:or
   (:&
    (from/stop-before
     drawer-start-line
     (:or stop-before-heading
          drawer-end))
    drawer-ish-comp-begin
    drawer-ish-comp-end)
   (:& (complement (:seq any-string (:or ":end:" ":END:") (:* " " "\t") (:+ (:~ (:or " " "\t" "\n")))))
       (from/stop-before (:seq drawer-start-line drawer-end-line) "\n"))))

(module+ test-drawer
  (define d-lexer
    (lexer-srcloc
     [drawer-ish (token 'DRAWER lexeme)]))

  (d-lexer (open-input-string "\n:b:\n:end:\n"))

  )

;; plain lists

(define-lex-abbrev bullet-marker (:or "." ")"))
(define-lex-abbrev marker-value (:or (:+ A-Z) (:+ a-z) (:+ 0-9)))


(define-lex-abbrev ordered-list-start
  ; wsnn+ or newline but can't eat the newline
  (:seq wsnn* marker-value bullet-marker))

(define-lex-abbrev descriptive-list-start
  (:or (:seq wsnn* (:or "-" "+")) (:seq wsnn+ "*")))

(define-lex-abbrev plain-list-start-helper
  (:or ordered-list-start descriptive-list-start))

(define-lex-abbrev plain-list-start
  (:or
   (:seq "\n" plain-list-start-helper wsnn+)
   (:&
    (:seq "\n" plain-list-start-helper)
    (from/stop-before (:seq "\n" plain-list-start-helper) "\n"))))

; marker types and sequences are completely interchangable, so the only
; useful distinction in the grammar is between ordered and descriptive
(define-lex-abbrev ordered-list-line
  (from/stop-before
   (:or
    (:seq "\n" ordered-list-start wsnn+)
    (:&
     (:seq "\n" ordered-list-start)
     (from/stop-before (:seq "\n" ordered-list-start) "\n")))
   "\n"))

(define-lex-abbrev descriptive-list-line
  (from/stop-before
   (:or
    (:seq "\n" descriptive-list-start wsnn+)
    (:&
     (:seq "\n" descriptive-list-start)
     (from/stop-before (:seq "\n" descriptive-list-start) "\n")))
   "\n"))

;; paragraphs

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
  (:+ paragraph-2) ; WARNING if you accidentally use this form and try
  ; to process the output with token-stop-for-paragraph you will it an
  ; infinite loop and massive memory usage as the tokenizer produces
  ; and infinite stream of empty strings

  #; ; this variant is broken and requires token-stop-for-paragraph, but we have very nearly
  ; constructed paragraph-1 such that it is the exact complement of all the other line elements
  ; NOW, having written this, we could try to use (complement all-the-other-elements) to implement
  ; paragraph ... worth investigating

  (from/stop-before
   paragraph-2
   ;(:or paragraph-2 paragraph-1)
   (:or "\n\n"
        ; FIXME this is broken because we have to look WAY ahead e.g. all the way to an #+end_src
        stop-before-heading ; FIXME shouldn't need this ??
        stop-before-footnote-def ; FIXME shouldn't need this ??
        ; plain-list-start ; FIXME we shouldn't need this ???
        ;stop-before-drawer-start
        drawer-start
        (:seq "\n" (:* (:or " " "\t")) "#+") ; keyword
        (:seq "\n" (:* (:or " " "\t")) "# ") ; comment
        ))
  #;
  (:+ (:seq "\n" (:seq (:* " " "\t")
                       (:+ (:or
                            lower-case
                            upper-case
                            0-9
                             ))
                       (:& (:~ "." ")") par-rest-ok)
                       (:+ par-rest-ok)))))

(define-lex-abbrev paragraph-2
  ; cases where we can safely match whole lines because they are followed by paragraph-1
  (:seq
   (:* ; this allows us to subsume paragraph-1
    (:seq
     "\n"
     (:+
      (:or
       (:+ (:or " " "\t"))
       (:+ lower-case)
       (:+ upper-case)
       (:+ 0-9)))))
   paragraph-1))

(define-lex-abbrev paragraph-1
  (:+
   (:or
    ;hyperlink ; already covered below
    ;hyperlink-ab ; already covered below
    ;timestamp ; FIXME including this here massively increases compile times and runtime and we don't need it at this stage
    (from/stop-before
     (:seq "\n"
           ;(:~ "*" "")
           (:* (:or " " "\t")) ; FIXME hits a nasty issue with "  #+end:" due to the leading whitespace shere somehow
           (:or
            (:or
             "~"
             "="
             "/"
             "_"
             "<" ">" ; anything that would collide here will be parsed in the nested paragraph parser
             "'"
             "\""
             "("
             ")"
             "]"
             ","
             ";" ; pretty sure this one is safe
             "!"
             "?"
             "@"
             "%"
             "$"
             "^"
             "&"
             ".")
            (:seq (:or "+" "-") (:~ whitespace)) ; FIXME somehow this fails to protect?
            (:seq wsnn+ "*" (:~ whitespace))
            #; ; broken ? or something else is wrong?
            (:seq (:+ "*") (:& (:~ whitespace) (:~ "*")))
            (:seq (:+ "*") (:~ (:or whitespace "*")))
            ; cases have to be paired to prevent (:~ "." ")") from further matching the same case
            ; FIXME TODO how to handle cases like \n123456789\n
            (:seq (:+ (:or " " "\t")) "[") ; footnote anchors and inline defs can start a paragraph line
            (:seq "[" (:~ "f"))
            (:seq "[f" (:~ "n"))
            (:seq "[fn" (:~ ":")) ; TODO see if we can get the actual complement of footnote label?
            (:seq (:+ A-Z) (:~ (:or bullet-marker A-Z "\n")))
            (:seq (:+ a-z) (:~ (:or bullet-marker a-z "\n")))
            (:seq (:+ 0-9) (:~ (:or bullet-marker 0-9 "\n")))
            (:seq
             (:or
              (:+ lower-case)
              (:+ upper-case)
              (:+ 0-9))
             (:or
              "-" "+" "*"
              (:seq
               bullet-marker
               (:~ whitespace))))))
     ; FIXME somehow keep the first of two newlines at the end? (per the spec trailing newlines go with the previous element)
     "\n"))))

(module+ test-par-1
  (require rackunit)
  (define p1-lexer
    (lexer-srcloc
     [paragraph-1 (token 'PARAGRAPH-1 lexeme)]))

  (define p2-lexer
    (lexer-srcloc
     [paragraph-2 (token 'PARAGRAPH-2 lexeme)]))

  (define p-lexer
    (lexer-srcloc
     [paragraph (token 'PARAGRAPH lexeme)]))

  (check-exn exn:fail? (λ () (p1-lexer (open-input-string "\n - \n"))))
  (check-exn exn:fail? (λ () (p2-lexer (open-input-string "\n - \n"))))
  (check-exn exn:fail? (λ () (p-lexer (open-input-string "\n - \n"))))

  (check-exn exn:fail? (λ () (p1-lexer (open-input-string "\n - g\n"))))
  (check-exn exn:fail? (λ () (p2-lexer (open-input-string "\n - g\n"))))
  (check-exn exn:fail? (λ () (p-lexer (open-input-string "\n - g\n"))))

  ; note the critical space after the f
  (p1-lexer (open-input-string "\n f \n - g\n"))
  (p2-lexer (open-input-string "\n f \n - g\n"))
  (check-equal? (token-struct-val (srcloc-token-token (p-lexer (open-input-string "\n f \n - g\n")))) "\n f ")

  (check-exn exn:fail? (λ () (p2-lexer (open-input-string "\n99.\n"))))

  (p2-lexer (open-input-string "\naaaaaaaaaaaaaaaaaaaaa\nx \n"))

  (p2-lexer (open-input-string "\nHello how are you?\n"))
  (p2-lexer (open-input-string "\nHello. How are you?\n"))

  )

;; org objects

;;; latex entities and fragments TODO there is more here that I have not implemented yet

(define-lex-abbrev latex-name
  (:seq "\\" (:+ alpha)))

(define-lex-abbrev latex-entity
  (:or
   (:seq latex-name "{}")
   (from/stop-before latex-name "\n")))

(define-lex-abbrev latext-$$ ; so the spec claims but I'm not sure I buy it
  (from/to "$$" "$$"))

(define-lex-abbrev latex-char
  (:~ (:or "." "," "?" ";" "'" "\"")))

(define-lex-abbrev latext-$
  (:or
   (from/stop-before
    "" (:seq (:or "\n" (:~ "$")) "$" latex-char "$" "\n"))
       (:seq (:or "\n" (:~ "$")) "$" latex-char "$" (:or "(){}[].,!\"\' "))))

(define-lex-abbrev latex-brackets
  (:*
   (:or
    (:seq "[" (:* (:~ (:or "\n" "[" "]" "{" "}"))) "]")
    (:seq "{" (:* (:~ (:or "\n" "{" "}"))) "}"))))

;;; export snippets

(define-lex-abbrev export-snip-start
  (:seq "@@" (:+ (:or alpha 0-9 "-")) ":"))

(define-lex-abbrev export-snip
  (from/to export-snip-start "@@")
  #;
  (from/stop-before
   export-snip-start
   (:or ; FIXME TODO interaction and priority relative to other
    ; elements, see the note on objects in the spec
    ; FIXME on review I think that our "paragraph" parser
    ; is actually a "may contain objects" parser and that
    ; export snips go in there
    stop-before-heading
    (:seq (:seq "@@" any-char)))))

;;; inline call

(define-lex-abbrev inline-call-header
  (:seq "[" (:* (:~ (:or "]" "\n"))) "]"))
(define-lex-abbrev inline-call-args
  (:seq "(" (:* (:~ (:or ")" "\n"))) ")"))

(define-lex-abbrev inline-call
  (:seq "call_"
        (:+ (:~ (:or "(" ")" "\n")))
        (:? inline-call-header)
        inline-call-args
        (:? inline-call-header)))

;;; inline src-block

(define-lex-abbrev inline-src-block ; XXX this seems underspecified in the spec
  ; e.g. what happens with src_name[oops I have more ]{of these}]{oh no!} src_name2[very oh no]  ]{echo lol}
  ; maybe that is ok
  (:seq "src_"
        (:+ (:~ whitespace))
        (:? (:seq "[" (:* (:~ "\n")) "]"))
        (:seq "{" (:* (:~ "\n")) "}")))

;;; line break

(define-lex-abbrev line-break ; otherwise non-empty ? that is a nasty lookbehind that we can't do here
  ; FIXME stop-before "" does not work
  (from/stop-before "" (:seq #;(:~ "\n") "\\" "\\" (:* (:or " " "\t")) "\n")))

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

#;
(define-lex-abbrev maybe-paired-square ; XXX does not work
  ; I think the best way to handle this is to use from/to in this way,
  ; and then check to see whether there are any extra left square
  ; brackets dangling, if there are then return paragraph instead of
  ; footnote, which I think is what the elisp implementation does
  (from/to "[" "]") ; this allows [[]
  #;
  (:or
   (:seq "[" "]")
   (:seq "[" "]")
   ;(from/to "[" (:or (:seq "[" ) "]"))
   ))

(define-lex-abbrev footnote-inline-start
  ; see footnote references
  ; the key constraint is that square brackets must be balanced
  ; which means that this portion cannot be handled in the tokenizer
  ; TODO technically these are allowed to contain only paragraph contents
  ; so not blocks or drawers
  (:seq "[fn:" (:? footnote-label) ":"))

; XXX broken
#;
(define-lex-abbrev footnote-inline-malformed ; FIXME -maybe-malformed, because it will match ok bits too but EOF causes issues
  ; FIXME verbatim issues probably

  ; FIXME arbitrary nesting cannot be handled effectively here

  ; FIXME this is incorrect, what we wanted was to detect cases where
  ; there was a double blank line (or was it single blank line?)
  ; before a RSB
  ; TODO this is going to need some special post processing
  ; yes footnotes are annoying to deal with due to need to match brackets
  ; we can't use from/stop-before for this because the gap will allow closing brackets to creep in
  #;
  (:seq footnote-inline-start (:* (:~ "]")) "\n\n") ; FIXME  double newline never makes it through because they end paragraphs in the top lexer
  (:seq footnote-inline-start (:seq (:+ (:seq (:* (:~ (:or "[" "]")) "\n")))
                                    ))
  #;
  (from/stop-before (:seq footnote-inline-start
                          (:* (:~ "]")))
                    (:or "]"
                         "\n\n")))

#; ; XXX fails in the [fn::a][fn::b] case contrast [fn::a[fn::b]]
(define-lex-abbrev footnote-inline-maybe
  (:seq footnote-inline-start (:*) "]"))

(define-lex-abbrev footnote-inline-simple
  (:seq footnote-inline-start
        (:or
         (:* (:~ "[" "]"))
         #; ; XXX this only handles a single level of nesting so is not a valid solution
         (:* (:seq "[" (:* (:~ "[" "]")) "]")))
        "]"))

(module+ test-fim
  (define fis-lexer
    (lexer-srcloc
     [footnote-inline-start (token 'FIS lexeme)]))

  (fis-lexer (open-input-string "[fn::Inline footnote[fn::Nested.].] more."))

  (define fisim-lexer
    (lexer-srcloc
     [footnote-inline-simple (token 'FISIM lexeme)]))

  (fisim-lexer (open-input-string "[fn::Inline footnote[fn::Nested.].] more."))

  #|
  (define fim-lexer
    (lexer-srcloc
     [footnote-inline-malformed (token 'FIM lexeme)]))

  (define fim+-lexer
    (lexer-srcloc
     [footnote-inline-malformed (token 'FIM lexeme)]
     [footnote-inline-start (token 'FIS lexeme)]))

  ; FIXME the double newline never makes it through the top level parser
  (fim-lexer (open-input-string "[fn::\n\n"))
  (fim+-lexer (open-input-string "[fn::\n\n"))
  (fim+-lexer (open-input-string "[fn:: sigh\n\n"))

  ; expect to fail
  (fim-lexer (open-input-string "[fn::Inline footnote[fn::Nested.].] more."))
  |#
  )

(define-lex-abbrev stop-before-footnote-def
   (:seq "\n" "[fn:" footnote-label "]"))

(define-lex-abbrev footnote-definition-line
  (from/stop-before
   (:seq
    "\n"
    (:or
     (:~ (:or "*" "[" "\n"))
     (:seq (:+ "*") (:~ (:or wsnn "*")))
     (:seq "[" (:~ (:or "f" "\n")))
     (:seq "[f" (:~ (:or "n" "\n")))
     (:seq "[fn" (:~ (:or ":" "\n")))
     (:seq "[fn:" (:~ (:or 0-9 alpha "_" "-" "\n"))) ; includes [fn:]
     (:seq "[fn:" (:* (:or 0-9 alpha "_" "-" "\n"))
           (:~ (:or 0-9 alpha "_" "-" "]" "\n")))))
   "\n"))

(define-lex-abbrev footnote-definition
  ; FIXME we can't use the (:& complement) technique here because these endings are technically not
  ; malformed, HOWEVER ... we might be able to use a positive definition instead ?

  (:seq
   (from/stop-before (:seq "\n" "[fn:" footnote-label "]") "\n")
   ; FIXME how to include the single newline
   (:*
    (:or (:seq "\n" (:+ footnote-definition-line))
         footnote-definition-line))
   )

  #; ; XXX this variant is extremely slow and doesn't actually work
  (:seq
   (from/stop-before (:seq "\n" "[fn:" footnote-label "]") "\n")
   (:+
    (:or paragraph
         drawer-ish
         table-element
         unknown-block
         keyword-element
         keyword-element-malformed))

   )

  ;(:&
  #; ; this appraoch doesn't work
  (:seq
   (from/stop-before (:seq "\n" "[fn:" footnote-label "]") "\n")
   (:*
    (:&
     (complement (:seq any-string "\n[fn:" footnote-label))
     ;(complement (from/stop-before stop-before-footnote-def "\n"))
     (complement (:seq any-string "\n" (:+ "*")))
     ;(complement (from/stop-before "\n" "\n\n"))
     ))
   )

  ; FIXME "word constituent characters" not 100% sure what that means in this and other similar contexts
  #;
  (from/stop-before
   (:seq "\n" "[fn:" footnote-label "]")
   (:or
    stop-before-footnote-def
    stop-before-heading
    "\n\n\n"
    ; eof as well it seems
    ;)
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

(define-lex-abbrev macro-invocation
  ; as opposed to macro definition via keywords
  ; FIXME the spec is clearly at divergence from elisp behavior because it allows newlines
  ; XXX divergece
  ; the closest pair of triple parents always parse as a macro regardless of whether their contents are well formed
  ; if their contents are NOT well formed then it should probably expand to be a malformed macro
  ; then we can determine whether default for that is to render as paragraph or render as nothing, paragraph probably better ...
  (from/to "{{{" "}}}"))

;;; target

(define-lex-abbrev target ; TODO further restrictions
  (:seq "<<" (:* (:~ "<" ">" "\n")) ">>"))

;;; radio target

(define-lex-abbrev radio-target ; TODO futher restrictions
  (:seq "<<<" (:* (:~ "<" ">" "\n")) ">>>"))

;;; stats cookies

; TODO decimal percent ? I'm unfamliar with this part of org
(define-lex-abbrev stats-percent (:seq "[" (:* 0-9) "%" "]"))

(define-lex-abbrev stats-quotient (:seq "[" (:* 0-9) "/" (:* 0-9) "]")) ; what the heck is [/0] or [0/] supposed to mean?

;;; subscript

(define-lex-abbrev subscript ; TODO
  "_")

;;; superscript

(define-lex-abbrev superscript ; TODO
  "^")

;;; timestamps ; FIXME somehow timestamp makes compile time long and runtime slow !?? (only if it is in paragraph-1 ???)
               ; FIXME in fact ... I wonder whether the grammar version of it was slowing down the grammar as well ...
               ; it works just fine as a standalone token but adds a bit of complexity to the grammar

(define-lex-abbrev year (:= 4 0-9))
(define-lex-abbrev year-ex (:seq (:or "+" "-") (:+ 0-9))) ; XXX divergence
(define-lex-abbrev month (:= 2 0-9))
(define-lex-abbrev day (:= 2 0-9))
(define-lex-abbrev day-abbrev (:+ (:~ (:or "+" "-" 0-9 "]" ">" "\n"))))

(define-lex-abbrev hour (:** 1 2 0-9)) ; spec says support for 1 digit hours, so we do, but always produce with leading zero
(define-lex-abbrev minute (:= 2 0-9))
(define-lex-abbrev second (:= 2 0-9))
(define-lex-abbrev subsecond (:+ 0-9)) ; don't bother limiting precision in the grammar

(define-lex-abbrev zoneoffset
  (:or "Z" ; you really really shouldn't use Z for auto-inserted, you should always use zoneoffset, Z is supported for other use cases
       (:seq (:or "+" "-") (:seq hour (:? ":") minute))))

(define-lex-abbrev date-suffix (:seq "-" month "-" day (:? (:seq (:+ (:or " " "\t")) day-abbrev))))
(define-lex-abbrev date-normal (:seq year date-suffix))
(define-lex-abbrev date-ex (:seq year-ex date-suffix))
(define-lex-abbrev date (:or date-normal date-ex))

(define-lex-abbrev time-normal (:seq hour ":" minute))
(define-lex-abbrev time-ex
  (:seq
   time-normal ; FIXME not happy about zoneoffset being optional, also not happy about earth/mars/moon/space issues
   ; FIXME EVEN MORE unhappy that hyphen minus was used as the separator because it is reused for negative zoneoffsets
   (:? (:seq ":" second (:? (:seq "," subsecond))))
   (:? zoneoffset)))
(define-lex-abbrev time (:or time-normal time-ex))

(define-lex-abbrev date?time (:seq date (:? (:seq wsnn+ time))))
(define-lex-abbrev datetime (:seq date wsnn+ time))

(define-lex-abbrev ts-rod-mark (:or "+" "++" ".+" "-" "--"))
(define-lex-abbrev ts-rod-value (:+ 0-9))
(define-lex-abbrev ts-rod-unit (char-set "hdwmy"))
(define-lex-abbrev ts-rod (:seq ts-rod-mark ts-rod-value ts-rod-unit))

(define-lex-abbrev ts-rod-02 (:** 0 2 ts-rod))

(define-lex-abbrev ts-diary
  (:seq "<%%(" (:* (:~ (:or ">" "\n")))")>"))
(define-lex-abbrev ts-active (:seq "<" date?time ts-rod-02 ">"))
(define-lex-abbrev ts-inactive (:seq "[" date?time ts-rod-02 "]"))
(define-lex-abbrev ts-range-active
  (:or (:seq ts-active "--" ts-active)
       (:seq "<" date wsnn+ time "-" time wsnn+ ts-rod-02 ">")))
(define-lex-abbrev ts-range-inactive
  (:or (:seq ts-inactive "--" ts-inactive)
       (:seq "[" date wsnn+ time "-" time wsnn+ ts-rod-02 "]")))

(define-lex-abbrev timestamp ; we use this at the top level since we will have to parse timestamps again later
  (:or ts-diary ts-active ts-inactive ts-range-active ts-range-inactive))

(module+ test-timestamp
  (define t-lexer
    (lexer-srcloc
     [timestamp (token 'TIMESTAMP lexeme)]))

  (t-lexer (open-input-string "<+01-01-01>"))
  (t-lexer (open-input-string "[+01-01-01]"))

  )

;;; markup


(define-lex-abbrev mu-pre-not-newline-not-lcb (:or " " "\t" "-" "(" "'" "\""))
(define-lex-abbrev mu-pre-1 (:or #;"\n" mu-pre-not-newline-not-lcb "{"))
#;
(define-lex-abbrev mu-pre (:+ mu-pre-1))
#;
(define-lex-abbrev mu-pre-n (:>= 2 mu-pre-1))
(define-lex-abbrev mu-pre-n-not-lcb (:or (:>= 2 mu-pre-not-newline-not-lcb) #;"\n"))
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
#;
(define-lex-abbrev markup-= (from/stop-before
                             (:seq "=" mu-border (:? (:seq (:* (:~ "="))
                                                           (:~ (:or whitespace "=")))) "=")
                             mu-post))

#;
(define-lex-abbrev markup-~ (from/stop-before
                             (:seq "~" mu-border (:? (:seq (:* (:~ "~"))
                                                           (:~ (:or whitespace "~")))) "~")
                             mu-post))

(define-syntax (define-markup stx)
  (syntax-parse stx
    [(_ delimiter:str)
     #:with (markup-delim-eof? markup-delim)
     (list
      (format-id #'delimiter #:source #'delimiter "markup-~a-eof?" (syntax-e #'delimiter))
      (format-id #'delimiter #:source #'delimiter "markup-~a" (syntax-e #'delimiter)))
     #; ; XXX have to use format-id for things to work in module+ apparently
     (string->symbol (format "markup-~a" (syntax-e #'delimiter)))
     #'(begin
         (provide markup-delim markup-delim-eof?)
         ; this should always be shorter than markup-delim so the markup-delim pattern should win
         ; and this should only appear at EOF
         (define-lex-abbrev markup-delim-eof?
           (:& (from/stop-before
                delimiter
                #; ; don't need this complex version now that we are using the intersectional approach
                (:seq delimiter (complement (:seq mu-border any-string)))
                (:seq
                 mu-border
                 delimiter
                 mu-post))
               (:seq delimiter mu-border any-string) ; ensure starts with delimiter
               (:>= 3 any-char) ; exclude degnerate cases
               ; ensure ends with delimiter to avoid eof issues
               (:seq any-string mu-border delimiter))
           #;
           (:& (from/to delimiter (:seq mu-border delimiter))
               (:seq delimiter mu-border any-string) ; ensure starts with delimiter
               (:>= 3 any-char) ; exclude degnerate cases
               ; ensure ends with delimiter to avoid eof issues
               (:seq any-string mu-border delimiter)))
         ; using token-back-1 in the lexer is required for this to work correctly
         (define-lex-abbrev markup-delim (:seq markup-delim-eof? mu-post)))]))

(define-markup "=")
(define-markup "~")

(define-markup "*")
(define-markup "/")
(define-markup "_")
(define-markup "+")

(module+ test-mu
  (define *-lexer
    (lexer-srcloc
     [markup-* (token 'BOLD lexeme)]))

  (*-lexer (open-input-string "*b /i _u +s =v /*_+lol+_*/= ~c /*_+lol+_*/~ s+ u_ i/ b*\n"))
  (*-lexer (open-input-string "*x* /z/ *y*\n"))
  (*-lexer (open-input-string "*x * /z/ *y*\n"))

  #; ; now fails correctly
  (*-lexer (open-input-string "* /z/ *y")) ; FIXME clearly wrong due to EOF isues

  #; ; now fails
  (*-lexer (open-input-string "* *"))

  #; ; fails as expected
  (*-lexer (open-input-string "*"))
  #; ; fails as expected
  (*-lexer (open-input-string "**"))
  (*-lexer (open-input-string "***\n"))

  (define =-lexer
    (lexer-srcloc
     [markup-= (token 'VERBATIM lexeme)]))

  (=-lexer (open-input-string "=x= z =y=\n"))


  (=-lexer (open-input-string "=x=\n"))
  (=-lexer (open-input-string "==x==\n"))

  )


#;
(define-lex-abbrev markup-=-eof? (:or (:seq (from/stop-before (:seq #;mu-pre "=" mu-border)
                                                              (:seq mu-border "=" #;mu-post))
                                            mu-border "=")
                                      (:seq #;mu-pre "=" mu-border "=")))

#;
(define-lex-abbrev markup-~-eof? (:or (:seq (from/stop-before (:seq #;mu-pre "~" mu-border) (:seq mu-border "~" #;mu-post)) mu-border "~")
                                      (:seq #;mu-pre "~" mu-border "~")))

; FIXME TODO investigate interactions between verbatim/code and other markup XXXXXXXXXX
; technically org immplements this by applying the paragraph parser repeatedly to the
; internal contents of markup, which is ... probably less than optimal to say the least

#; ; no longer used
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
                              #`(from/stop-before
                                 (:seq #,mut mu-border (:? (:seq (:* (:~ #,stop)) mu-border)) #,stop)
                                 mu-post)
                              #;
                              #`(:or (:seq (from/stop-before (:seq #;mu-pre #,mut mu-border)
                                                             (:seq mu-border #,stop mu-post))
                                           mu-border
                                           #,stop)
                                     (from/stop-before (:seq #;mu-pre #,mut mu-border #,stop) mu-post)))))
                #;
                (list 'provide (string->symbol (format "markup-~a-eof?" c)))
                #;
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

#;
(make-markup-abbrevs)

;; other parser bits

(define-lex-abbrev section (from/stop-before heading heading)) ; FIXME BOF EOF issues
