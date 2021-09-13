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
(define-lex-abbrev heading (:seq "\n" (:+ "*") wsnn (:* (:~ "\n"))))


(define-lex-abbrev plan-dead (:or "DEADLINE" "deadline"))
(define-lex-abbrev plan-sched (:or "SCHEDULED" "scheduled"))
(define-lex-abbrev plan-open (:or "OPENED" "opened"))
(define-lex-abbrev plan-close (:or "CLOSED" "closed"))
(define-lex-abbrev plan-keyword (:seq (:or plan-dead plan-sched plan-open plan-close)))
;(define-lex-abbrev plan-info (:seq plan-keyword (:or (:? (:seq wsnn* timestamp wsnn*)) wsnn+))) ; doesn't quite solve SCHEDULED:DEADLINE: with eol
(define-lex-abbrev plan-info (:seq plan-keyword wsnn* (:? timestamp)))
(define-lex-abbrev planning-line           (:seq "\n" (:+ (:seq wsnn* plan-info)) wsnn*))
(define-lex-abbrev planning-line-malformed (:seq "\n" (:+ (:seq wsnn* plan-info)) wsnn* (:~ whitespace) (:+ (:~ "\n"))))

(define-lex-abbrev comment-element
  (:+ (:seq "\n" wsnn* "#" (:seq wsnn (:* (:~ "\n"))))))

(define-lex-abbrev runtime-todo-keyword (:or "TODO" "DONE")) ; FIXME SIGH SIGH SIGH

;;; keywords

(define-lex-abbrev keyword-element ; FIXME broken for #+begin_: I think but also consider that maybe keyword should take precendence in this case?
  (from/stop-before
   (:seq "\n"
         wsnn*
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
        wsnn*
        "#+"
        (:or "begin" "BEGIN" "end" "END")
        "_"
        (:~ wsnn)
        any-string))
      (:seq
       "\n"
       wsnn*
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

(define-lex-abbrev stop-before-heading (:seq "\n" (:+ "*") wsnn))

;;;; latex environments

(define-lex-abbrev latex-environment-start
  ; these are like greater blocks and require runtime generated lexers
  ; therefore there is no end pattern defined here
  (:seq "\begin{" (:+ (:or alpha 0-9 "*")) "}"))

;;;; blocks

(define-lex-abbrev src-block-line-begin
  (:seq "\n"
        wsnn*
        (:or "#+begin_src" "#+BEGIN_SRC")
        (:* (:~ "\n"))))

(define-lex-abbrev src-block-line-end
  (:seq "\n" wsnn* (:or "#+end_src" "#+END_SRC") wsnn*))

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
        wsnn*
        (:or (:seq "#+begin_"
                   (:+ (:~ whitespace))
                   (:?
                    (:seq
                     wsnn
                     (:+ (:~ "\n")))))
             (:seq "#+BEGIN_"
                   (:+ (:~ whitespace))
                   (:?
                    (:seq
                     wsnn
                     (:+ (:~ "\n"))))))))

(define-lex-abbrev unknown-block-line-end
  (:seq "\n"
        wsnn*
        (:or (:seq "#+end_"
                   (:+ (:~ whitespace))
                   (:? (:seq
                        wsnn
                        (:* (:~ "\n")))))
             (:seq "#+END_"
                   (:+ (:~ whitespace))
                   (:? (:seq
                        wsnn
                        (:* (:~ "\n"))))
                   ))
        wsnn*))

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

;;;; dynamic blocks

(define-lex-abbrev dynamic-block-start-line
  (:seq "\n" wsnn* "#+" (:or "begin" "BEGIN") ":" ))

(define-lex-abbrev dynamic-block-start
  (:seq dynamic-block-start-line "\n"))

(define-lex-abbrev dynamic-block-end-line
  (:seq "\n" wsnn* "#+" (:or "end" "END") ":" wsnn*))

(define-lex-abbrev dynamic-block-end
  (:seq dynamic-block-end-line "\n"))

(define-lex-abbrev dynamic-block-comp-begin
  (complement (:seq dynamic-block-start-line wsnn* (:+ (:~ whitespace)))))

(define-lex-abbrev dynamic-block-comp-end
  (complement (:seq any-string "\n" wsnn* (:or "#+end:" "#+END:") wsnn* (:+ (:~ whitespace)))))

(define-lex-abbrev dynamic-block
  (:or
   (:&
    (from/stop-before
     dynamic-block-start-line
     (:or stop-before-heading
          dynamic-block-end))
    (complement (:seq any-string "\n"))
    (complement (:seq any-string "\n" (:+ "*")))
    dynamic-block-comp-begin
    dynamic-block-comp-end)
   (:& (complement (:seq any-string (:or "#+end:" "#+END:") wsnn* (:+ (:~ whitespace))))
       (from/stop-before (:seq dynamic-block-start-line dynamic-block-end-line) "\n"))))

;;;; drawers


(define-lex-abbrev drawer-end-comp
  (complement (:seq any-string (:or ":end:" ":END:") wsnn* (:+ (:~ whitespace)))))

(define-lex-abbrev drawer-prop-comp
  (complement (:seq "\n" wsnn* ":properties:" wsnn* (:+ (:~ whitespace)))))

(define-lex-abbrev drawer-PROP-comp
  (complement (:seq "\n" wsnn* ":PROPERTIES:" wsnn* (:+ (:~ whitespace)))))

(define-lex-abbrev drawer-props
  (:or (:& (from/stop-before
            (:seq "\n" wsnn* ":properties:" wsnn*)
            ; FIXME NOTE there are cases where the pattern in the
            ; names of the property values for a draw cause it to NO
            ; LONGER BE a property drawer, this is almost certainly a
            ; design flaw in org mode which makes it difficult to
            ; parse the property drawers correctly and efficiently in
            ; this way, the other possibility is to accept the more
            ; complex grammar that we already wrote which more or
            ; less works as expected at the expense of performance
            (:or stop-before-heading
                 (:seq "\n" wsnn* ":end:" wsnn* "\n")))
           ; I don't understand how the variant above matches things where :end: has 
           ; something other than whitespace in front of it :/
           drawer-prop-comp
           (:seq any-string "\n" wsnn* ":end:" wsnn*))
       ; sigh legacy support for this
       (:& (from/stop-before
            (:seq "\n" wsnn* ":PROPERTIES:" wsnn*)
            (:or stop-before-heading
                 (:seq "\n" wsnn* ":END:" wsnn* "\n")))
           drawer-PROP-comp
           (:seq any-string "\n" wsnn* ":END:" wsnn*))
       (:& drawer-end-comp
        (from/stop-before
         (:or
          (:seq "\n" wsnn* ":properties:" wsnn* "\n" wsnn* ":end:" wsnn*)
          (:seq "\n" wsnn* ":PROPERTIES:" wsnn* "\n" wsnn* ":END:" wsnn*))
         "\n"))))

(define-lex-abbrev drawer-start-line
  (:& (complement (:seq "\n" wsnn* (:or ":end:" ":END:") wsnn*))
      (:seq "\n" wsnn* ":" (:+ (:or 0-9 alpha "-" "_")) ":" wsnn*)))

(define-lex-abbrev drawer-start
  (:seq drawer-start-line "\n"))

(define-lex-abbrev drawer-end-line
  (:seq "\n" (:* " " "\t") ":end:" (:* " " "\t")))

(define-lex-abbrev drawer-end
  (:seq drawer-end-line "\n"))

(define-lex-abbrev drawer-ish-comp-begin
  ; FIXME seq this any-string?
  (complement (:seq "\n" wsnn* ":" (:+ (:or 0-9 alpha "-" "_")) ":" wsnn*
                    (:+ (:~ whitespace)))))

(define-lex-abbrev drawer-ish-comp-end
  (:or #; (:seq any-string "\n" (:+ "*")) ; FIXME what happens if we leave this out ? TODO apparently it correctly does not match as a drawer?
       (:seq any-string "\n" wsnn* (:or ":end:" ":END:") wsnn*)))

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
   (:& (complement (:seq any-string (:or ":end:" ":END:") wsnn* (:+ (:~ whitespace))))
       (from/stop-before (:seq drawer-start-line drawer-end-line) "\n"))))

(module+ test-drawer
  (define d-lexer
    (lexer-srcloc
     [drawer-ish (token 'DRAWER lexeme)]))

  (d-lexer (open-input-string "\n:b:\n:end:\n"))

  )

;; plain lists

(define-lex-abbrev bullet-counter (:or (:+ A-Z) (:+ a-z) (:+ 0-9)))
(define-lex-abbrev bullet-marker (:or "." ")"))


(define-lex-abbrev ordered-list-start
  ; wsnn+ or newline but can't eat the newline
  (:seq wsnn* bullet-counter bullet-marker))

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
   (:or
    (from/stop-before (:seq "\n" descriptive-list-start wsnn+) "\n")
    (:&
     (:seq "\n" descriptive-list-start)
     ; use stop-before ensures that we don't go too far, but it does not ensure that we don't stop too soon
     (from/stop-before (:seq "\n" descriptive-list-start) "\n")))
   )

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
       wsnn+
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
           wsnn* ; FIXME hits a nasty issue with "  #+end:" due to the leading whitespace shere somehow
           (:or
            (:or
             "~"
             "="
             "/"
             "_"
             "<" ">" ; anything that would collide here will be parsed in the nested paragraph parser
             "'"
             "\""
             "(" ")"
             "{" "}"
                 "]" ; no left square bracket due to footnote definitions
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
            (:seq wsnn+ "[") ; footnote anchors and inline defs can start a paragraph line
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

(define-lex-abbrev latex-entity/fragment-1
  ; XXX note that some of these may in fact be fragments at runtime
  (:or
   ; XXX the spec is incomplete here because it does not indicate whether
   ; entities are shortest match or whether \name{}{} always parses as a fragment
   ; I am going to rul that \name{}{} always parses as a fragment which means that
   ; the spec is missing a note that a fragment that ends in {} cannot be followed by [] or {}
   ; (not that you would implement it in that way) NOW, this may also be incorrect because
   ; someone could do \pi{}[lol look at me] but I'm not entirely sure about that should happen there
   (:seq latex-name "{}")
   (:&
    latex-name
    (from/stop-before latex-name "\n"))))

(define-lex-abbrev latex-brackets-curley*
  (:seq "{" (:* (:~ (:or "\n" "{" "}"))) "}"))
(define-lex-abbrev latex-brackets-curley+
  (:seq "{" (:+ (:~ (:or "\n" "{" "}"))) "}"))
(define-lex-abbrev latex-brackets-square
  (:seq "[" (:* (:~ (:or "\n" "[" "]" "{" "}"))) "]"))

(define-lex-abbrev latex-brackets
  ; curley+ differentiates \name{contents*} from \name{contents+} and
  ; :>= 2 differentiates \name{} from \name{}{} and \name{}[]
  (:?
   (:or
    latex-brackets-curley+
    latex-brackets-square
    (:>= 2
         (:or
          latex-brackets-curley*
          latex-brackets-square)))))

(define-lex-abbrev latex-fragment-n
  ; XXX collides with latex-entity, in the grammar they need to be combined sort it out afterward
  (:seq latex-name latex-brackets))

(define-lex-abbrev latex-fragment-parens
  (:or
   ; this pattern is insurance against accidental greedy behavior
   (:seq (from/stop-before "\\[" "\\]") "]")
   (:seq (from/stop-before "\\(" "\\)") ")")
   ))

; the $ and $$ forms are not implemented due to the note from ngz
#;
(define-lex-abbrev latext-$$ ; so the spec claims but I'm not sure I buy it
  (from/to "$$" "$$"))

#;
(define-lex-abbrev latex-char
  (:~ (:or "." "," "?" ";" "'" "\"")))

#;
(define-lex-abbrev latext-$
  (:or
   (from/stop-before
    "" (:seq (:or "\n" (:~ "$")) "$" latex-char "$" "\n"))
   (:seq (:or "\n" (:~ "$")) "$" latex-char "$" (:or "(){}[].,!\"\' "))
   ))

;;; export snippets

(define-lex-abbrev export-snip-start
  (:seq "@@" (:+ (:or alpha 0-9 "-")) ":"))

(define-lex-abbrev export-snip
  (:seq (from/stop-before export-snip-start "@@") "@") ; insurance against greedy
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

;;; delimiter extension

; pair these with (from/stop-before "" (:seq "]" pattern "|")) at tokenize time like we did for source blocks
; that way if a user needs to nest, they can just bump the escape sequence (yay induction)
(define-lex-abbrev extend-delim-start (:seq "|" (:* (:or (:~ whitespace alpha 0-9))))) ; TODO check the racket syntax
(define-lex-abbrev extend-curlie-start (:seq extend-delim-start "{"))
(define-lex-abbrev extend-square-start (:seq extend-delim-start "["))
(define-lex-abbrev extend-parens-start (:seq extend-delim-start "("))

;;; inline call

(define-lex-abbrev inline-call-header
  (:seq "[" (:* (:~ (:or "]" "\n"))) "]"))
(define-lex-abbrev inline-call-args
  (:seq "(" (:* (:~ (:or ")" "\n"))) ")"))

(define-lex-abbrev inline-call-start
  ; XXX spec is underspecified, call_oops why do I have [a](b=c)[d] in my name [:eval never](a=1, b=2)[:post lol]
  ; as written the spec would gobble badly call_name()[]() call_name2()[]()
  ; XXX that thing about allowing spaces in names ...
  #; ; dirty approach
  (from/stop-before
   (:seq "call_" (:+ (:~ (:or "(" ")" "\n")))) ; XXX greedy vs non-greedy
   (:or "[" "("))
  ; a cleaner approach
  (:seq
   "call_"
   (:+ (:~ (:or "[" "]" "(" ")" "\n")))))

(define-lex-abbrev inline-call
  (:seq "call_"
        (:+ (:~ (:or "(" ")" "\n"))) ; XXX that thing about allowing spaces in names ...
        (:? inline-call-header)
        inline-call-args
        (:? inline-call-header)))

;;; inline src-block

; FIXME TODO I think that we are going to need a generic solution for
; delimiters because the spec is inconsistent in too many cases and
; there are a bunch of bugs when different objects interact

(define-lex-abbrev inline-src-block-start
  (:seq "src_" (:+ (:~ whitespace "[" "]" "{" "}"))))

(define-lex-abbrev inline-src-block ; XXX this seems underspecified in
  ; the spec XXX yes, it is under specified and the actual behavior is
  ; radically different, requiring matched parens and allowing
  ; newlines

  ; for now I'm going to do this as shortest match which
  ; effectively means that rcb is not allowed

  ; e.g. what happens with src_name[oops I have more ]{of these}]{oh no!} src_name2[very oh no]  ]{echo lol}
  ; maybe that is ok
  (:seq "src_"
        (:+ (:~ whitespace))
        (:? (:seq "[" (:* (:~ "\n")) "]"))
        ; FIXME while this is what the spec says, it produces longest
        ; matches, which break usage inside {} script this is another
        ; case where it is hard to know what the best solution is
        ; because the spec is internally inconsistent and doesn't
        ; match the actual behavior, ox and babel both get it right
        (:seq "{" (:* (:~ "\n")) "}")))

;;; line break

(define-lex-abbrev line-break ; otherwise non-empty ? that is a nasty lookbehind that we can't do here
  ; FIXME stop-before "" does not work
  (from/stop-before "" (:seq #;(:~ "\n") "\\" "\\" wsnn* "\n")))

;;; citation

(define-lex-abbrev citation-key
  (:seq "@" (:+ (:or 0-9 A-Z a-z "-.:?!`'/*@+|(){}<>&_^$#%&~"))))

(define-lex-abbrev citation
  (:seq
   (from/stop-before ; insurance against greedy match
    (:seq "[cite" (:* (:seq "/"
                            ; FIXME I think this is wrong because it will match [cite//:] ?
                            (:+ (:or 0-9 alpha "-" "_" "/"))))
          ":"
          (:* whitespace))
    ; TODO internal citation structure parser
    ; FIXME I think we need to stop before heading here as well?
    ; because [cite:\n* @key] would match this I think ...?
    "]")
   "]"))

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
  (:seq "[fn:" (:? footnote-label) ":"
        #; ; NOPE this makes things irregular
        ; there is no reasonable solution here as far as I can tell
        ; the right way to solve this is to update the elisp implementation
        ; so that markup takes priority over closing delimiters, the reason
        ; elisp can get away with this is because they use a hack in the tokenizer
        ; to fake a the nesting, but if you do it for real in the grammar you
        ; realize that there will be a conflict between the markup and the nesting
        ; and you really really want the markup to be handled by the tokenizer and
        ; not part of the grammar, because markup in the grammar is a nightmare
        ; there might be another way out of this but I'm not seeing it
        ; XXX this divergence is ok because it is technically a relaxing of a constraint
        ; even then there are still differences in behavior but edge cases
        footnote-inline-segment))

(define-lex-abbrev footnote-inline-segment
  (:* (:~ (:or "[" "]")))
  )

#; ; can't use this because the grammar has to do the counting
(define-lex-abbrev footnote-inline-starts
  (:+ footnote-inline-start)
  )

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
  ; we can't use the (:& complement) technique here because these
  ; endings are technically not malformed, thus the constructive
  ; approach
  (:seq
   (from/stop-before (:seq "\n" "[fn:" footnote-label "]") "\n")
   ; FIXME how to include the single newline
   (:*
    (:or
     (:seq "\n" (:+ footnote-definition-line))
     footnote-definition-line))))

;;; left square bracket

#;
(define-lex-abbrev left-square-bracket-helper
  #; ; this eats everything
  (:& (:seq "[" any-string)
      (complement
       (:seq
        (:or timestamp
             footnote-inline-start
             citation
             hyperlink)
        any-string))
      )
  ; XXX incomplete
  #;
  (:seq "[" (:or (:~ (:or "c" "f" "[" 0-9))))
  (:& "["
      (complement
       (:seq
        (:or
         ts-inactive
         ts-range-inactive
         footnote-inline-start
         citation)
        any-string))))

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

  #; ; FIXME this is NOT doing shortest match ! somehow from/to is doing a greedy match ??? good to known
  (from/to (:seq "{{{" alpha) "}}}")

  #; ; FIXME also fails due to matchin 4 nested as {{{{}}}
  (:seq (from/stop-before "{{{" "}}}") "}")
  (:seq (from/stop-before (:seq "{{{" alpha) "}}}") "}")
  )

(module+ test-macro
  (require rackunit)
  (define m-lexer
    (lexer-srcloc
     [macro-invocation (token 'MACRO lexeme)]))
  (m-lexer (open-input-string "{{{a}}}}"))

  (check-exn exn:fail? (λ () (m-lexer (open-input-string "{{{{}}}}"))))
  (check-exn exn:fail? (λ () (m-lexer (open-input-string "{{{{ }}}}"))))
  )

;;; noweb target

(define-lex-abbrev noweb-target ; TODO further restrictions
  (:seq "<<" (:* (:~ "<" ">" "\n")) ">>"))

;;; radio target

(define-lex-abbrev radio-target ; TODO futher restrictions
  (:seq "<<<" (:* (:~ "<" ">" "\n")) ">>>"))

;;; stats cookies

; TODO decimal percent ? I'm unfamliar with this part of org
(define-lex-abbrev stats-percent (:seq "[" (:* 0-9) "%" "]"))

(define-lex-abbrev stats-quotient (:seq "[" (:* 0-9) "/" (:* 0-9) "]")) ; what the heck is [/0] or [0/] supposed to mean?

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

(define-lex-abbrev date-suffix (:seq "-" month "-" day (:? (:seq wsnn+ day-abbrev))))
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

;;; subscript

; XXXXXXXXXXXXXXXXXXXXXXX I'm going to suggest that for the standard '|{}| be made the default behavior
; for org-export-with-sub-superscripts for tier 1 conforming implementations defaulting to t leads to
; 99% of files being rendered incorrectly because users never configure this behavior and the result
; is surprising and undesireable in nearly every case e.g. when generating documentation

(define-lex-abbrev script-body
  ; FIXME this should be off by default, we can match it but should
  ; return a non-latex token unless a parameter is set
  (:or
   ; org-match-substring-regexp
   "*" ; this does not conflict with bold because the markup could not trigger
   (:seq
    (:? (:or "+" "-"))
    (:* (:or alpha 0-9 "," "." "\\"))
    (:or alpha 0-9))))


(define-lex-abbrev script-marker (:or "_" "^"))

(define-lex-abbrev script-body-start-b "{")
(define-lex-abbrev script-body-start-p "(")

; FIXME AAAAAAAAAAAAAAAAAAAAAAAAAAAA nested/balanced () and {} argh so all the right delimiters must be separate
; XXX note that per org-match-sexp-depth and org-create-multibrace-regexp
; these are not actually nested, they are artifically cut off at a nesting depth of 3

; FIXME the :~ whitespace has to be in the grammar
(define-lex-abbrev subscript (:seq #;(:~ whitespace) "_" script-body))
(define-lex-abbrev subscript-start-b (:seq #;(:~ whitespace) "_" script-body-start-b))
(define-lex-abbrev subscript-start-p (:seq #;(:~ whitespace) "_" script-body-start-p))

;;; superscript

(define-lex-abbrev superscript (:seq #;(:~ whitespace) "^" script-body))
(define-lex-abbrev superscript-start-b (:seq #;(:~ whitespace) "^" script-body-start-b))
(define-lex-abbrev superscript-start-p (:seq #;(:~ whitespace) "^" script-body-start-p))


;;; script markup interaction

; subsets we need are
; set before script but not before markup
; set before script and set before markup
; set before markup but not before script

#; ; bad
(define-lex-abbrev pre+s-m ; also don't use this one because it will gobble
  (:& (:~ whitespace)
      ; things we have to exclude to avoid gobbling other critical elements that we will then need to restore in the grammar
      (:~ (:or script-marker mu-marker
               "{" "}"
               "(" ")"
               #; "[" "]"
               "<" ">"
               ))
      (:~ (:or "-" "{" "(" "'" "\""))))

(define-lex-abbrev pre+s+m
  (:or "-" "{" "(" "'" "\""))

(define-lex-abbrev pre-s+m ; don't use this, it is just for record keeping, should go in the grammar
  wsnn)

;;; markup

(define-lex-abbrev mu-pre-safe-1 ; subset with no participation in other places
  (:or "-" "'" "\"")) 

(define-lex-abbrev mu-pre-safe
  (:+ mu-pre-safe-1))

(define-lex-abbrev mu-pre-1 ; still missing the whitespace
  (:or mu-pre-safe-1 "{" "("))

#;
(define-lex-abbrev mu-pre-not-newline-not-lcb (:or " " "\t" "-" "(" "'" "\""))
#;
(define-lex-abbrev mu-pre-1 (:or #;"\n" mu-pre-not-newline-not-lcb "{"))


#;
(define-lex-abbrev mu-pre (:+ mu-pre-1))
#;
(define-lex-abbrev mu-pre-n (:>= 2 mu-pre-1))
#;
(define-lex-abbrev mu-pre-n-not-lcb (:or (:>= 2 mu-pre-not-newline-not-lcb) #;"\n"))
; FIXME mu-post can also be newline, which is a problem due to newline
; first from/stop-before solves this
(define-lex-abbrev mu-post-less-whitespace-delim (:or "[" "-" "." "," ";" ":" "!" "?" "'" "\""))
(define-lex-abbrev mu-post-1 (:or ")" "}" #;"[" whitespace mu-post-less-whitespace-delim))
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
                 mu-post-1))
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
         (define-lex-abbrev markup-delim
           ; XXX sadly it seems that we still have to step back 1 here
           (:seq markup-delim-eof? mu-post-1)
           #; ; XXX you cannot peek this because it will just stop short and match, and then not backtrack
           (:& markup-delim-eof?
            (from/stop-before
              markup-delim-eof?
              mu-post-1))))]))

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

;; other parser bits

(define-lex-abbrev section (from/stop-before heading heading)) ; FIXME BOF EOF issues
