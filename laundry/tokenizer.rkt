#lang racket/base
(require racket/pretty)
(require brag/support)
(provide laundry-make-tokenizer
         paragraph-make-tokenizer

         heading
         hyperlink
         comment-element
         drawer-ish

         paragraph
         markup-*
         markup-/
         markup-_
         markup-+
         markup-=
         markup-~
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
(define-lex-abbrev hyperlink (:seq "[[" (:+ (:~ "\n")) "]]"))

#; ; can't use this variant because it gobbles \n# in the \n#+name: case
(define-lex-abbrev comment-element (:+ (:seq "\n" (:* " " "\t") "#" (:* (:seq (:or " " "\t") (:* (:~ "\n")))))))
(define-lex-abbrev comment-element (:+ (:seq "\n" (:* " " "\t") "#" (:seq (:or " " "\t") (:* (:~ "\n"))))))

(define-lex-abbrev drawer-ish
  (from/to (:seq "\n" (:* " " "\t") ":" (:+ (:or 0-9 alpha "-" "_")) ":") ; FIXME this is not right, check the spec to see
           (:seq "\n" (:* " " "\t") ":end:")))

(define-lex-abbrev par-start (:or alpha 0-9 " " "\t"))
(define-lex-abbrev par-rest-ok (:or par-start "-" "(" ")" "." "," "?" "!" "'" "\""
                                    ; "*" "/" "_" "+" "=" "~" ; markup markers can't be included
                                    "|" "\\" "^" ";"))
; parsing paragraph here in this way is an optimization that is absolutely necessary for performance
; in order to avoid some quadratic algorithm lurking somewhere in the grammar or in brag or in parser-tools
#;
(define-lex-abbrev paragraph (:+ (:seq "\n" par-start (:* (:or par-start par-rest-ok)) (:+ (:or par-start par-rest-ok)))))
(define-lex-abbrev paragraph
  (:+ (:seq "\n" (:seq (:* " " "\t")
                       (:or (:+ lower-case)
                            (:+ upper-case)
                            (:+ 0-9))
                       (:& (:~ "." ")") par-rest-ok)
                       (:+ par-rest-ok)))))
#;
(define-lex-abbrev paragraph (:seq "\n" (:+ (:seq (:+ (:or alpha 0-9 "," " " "\t") (:** 1 2 "\n")) (:or alpha 0-9 " " "\t")))))
#; ; too complex ?
(define-lex-abbrev src-block (:seq "\n" (:* (:or " " "\t")) "#+begin_src" (:** 0 1 (:seq (:or " " "\t") (:~ "\n")))
                                   (:* (:seq "\n"
                                             (:or (:seq (:~ "*") (:+ (:~ "\n")))
                                                  (:seq (:+ "*") (:~ "*" " " "\t" "\n")))))
                                   (:* "\n" " " "\t") "#+end_src" (:* " " "\t")))

(define-lex-abbrev whitespace (:or "\n" "\t" " ")) ; pretty sure this is complete
(define-lex-abbrev mu-pre (:or whitespace "-" "(" "{" "'" "\""))
; FIXME mu-post can also be newline, which is a problem due to newline
; first from/stop-before solves this
(define-lex-abbrev mu-post (:or "\n" "\t" " " "-" "." "," ";" ":" "!" "?" "'" ")" "}" "[" "\""))
(define-lex-abbrev mu-border (:~ whitespace))
(define-lex-abbrev mu-marker (:or "*" "/" "_" "+" "=" "~"))
; using from/stop-before and not worrying about the 3 line limit due to its increased complexity
; XXX the issue is that this eats any number of other things that have higher priority, such as
; headlines, so I'm 99% sure that this has to be done in a second pass that only applies to paragraphs
(define-lex-abbrev markup-* (:seq (from/stop-before (:seq mu-pre "*" mu-border) (:seq mu-border "*" mu-post)) "*"))
(define-lex-abbrev markup-/ (:seq (from/stop-before (:seq mu-pre "/" mu-border) (:seq mu-border "/" mu-post)) "/"))
(define-lex-abbrev markup-_ (:seq (from/stop-before (:seq mu-pre "_" mu-border) (:seq mu-border "_" mu-post)) "_"))
(define-lex-abbrev markup-+ (:seq (from/stop-before (:seq mu-pre "+" mu-border) (:seq mu-border "+" mu-post)) "+"))
(define-lex-abbrev markup-= (:seq (from/stop-before (:seq mu-pre "=" mu-border) (:seq mu-border "=" mu-post)) "="))
(define-lex-abbrev markup-~ (:seq (from/stop-before (:seq mu-pre "~" mu-border) (:seq mu-border "~" mu-post)) "~"))

(define-lex-abbrev src-block (:seq "#+begin_src" (:+ (:~ "*")) "#+end_src"))
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

(define paragraph-lexer
  (lexer-srcloc
   [(:+ (:~ mu-pre mu-marker)) (token 'STUFF lexeme)]
   [markup-* (token 'BOLD lexeme)]
   [markup-/ (token 'ITALIC lexeme)]
   [markup-_ (token 'UNDERLINE lexeme)]
   [markup-+ (token 'STRIKE lexeme)]
   [markup-= (token 'VERBATIM lexeme)]
   [markup-~ (token 'CODE lexeme)]
   [(:+ (:or mu-pre mu-marker)) (token 'STUFF lexeme)])
  #;
  (
   ["\n" (token 'NEWLINE)]
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
   ["TODO" (token 'RUNTIME-TODO-KEYWORD lexeme)]
   ["DONE" (token 'RUNTIME-TODO-KEYWORD lexeme)]

   [heading (token 'HEADING lexeme)] ; brilliant! because this is parsed first the from/to works as expected (HAH)

   ; FIXME there is a question of whether to use the lexer like this
   ; to slurp code blocks in the first pass and then run code block
   ; specific parser in a later pass, or whether we want to try to
   ; get certain information in the first pass
   [(from/to (:seq "\n" (:* " " "\t") "#+begin_src")
             (:seq "\n" (:* " " "\t") "#+end_src" (:* " " "\t")))
    (token 'SRC-BLOCK lexeme)]
   ;[src-block (token 'SRC-BLOCK lexeme)]

   [hyperlink (token 'LINK lexeme)] ; as it turns out this also helps performance immensely
   ; in theory it should be possible to scan for headlines and then parse all the sections
   ; in parallel

   [comment-element (token 'COMMENT-ELEMENT lexeme)]

   [(from/to (:seq "\n" (:* " " "\t") ":properties:")
             ; FIXME NOTE there are cases where the pattern in the
             ; names of the property values for a draw cause it to NO
             ; LONGER BE a property drawer, this is almost certainly a
             ; design flaw in org mode which makes it difficult to
             ; parse the property drawers correctly and efficiently in
             ; this way, the other possibility is to accept the more
             ; complex grammar that we already wrote which more or
             ; less works as expected at the expense of performance
             (:seq "\n" (:* " " "\t") ":end:" (:* " " "\t")))
    (token 'DRAWER-PROPS lexeme)]
   ; sigh legacy support for this
   [(from/to (:seq "\n" (:* " " "\t") ":PROPERTIES:")
             (:seq "\n" (:* " " "\t") ":END:" (:* " " "\t")))
    (token 'DRAWER-PROPS lexeme)]

   [drawer-ish (token 'DRAWER lexeme)]

   [paragraph (token 'PARAGRAPH lexeme)] ; needed for performance reasons to mitigate quadratic behavior around short tokens


   ["*" (token 'ASTERISK lexeme)]
   [(:>= 2 "*") (token 'STARS lexeme)] ; need this in lexer otherwise performance tanks in the parser
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

   ; FIXME 99% these should not be in the tokenizer maybe with #+NAME: #+name:
   ; the issue is that if these appear at other places in the file we will be
   ; in trouble ; NOTE have to add the colons ourselves where needed
   ; XXX eh, we've more or less gotten it all worked out here
   ; affiliated keywords
   [(:or "#+NAME" "#+name") (token 'NAME lexeme)]
   [(:or "#+HEADER" "#+header") (token 'HEADER lexeme)]
   [(:or "#+PLOT" "#+plot") (token 'PLOT lexeme)]
   [(:or "#+AUTHOR" "#+author") (token 'AUTHOR lexeme)]
   [(:or "#+DATE" "#+date") (token 'DATE lexeme)]
   [(:or "#+TITLE" "#+title") (token 'TITLE lexeme)]
   [(:or "#+RESULTS" "#+results") (token 'RESULT lexeme)] ; NOTE THE PLURAL
   [(:or "#+CAPTION" "#+caption") (token 'CAPTION lexeme)]
   [(:or "#+ATTR_" "#+attr_") (token 'ATTR-PREFIX lexeme)]
   
   ; the call ... not actually keyword and not actually associated
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
  (define bof (= (file-position port) 0))
  (define (next-token)
    (if bof ; sigh branches instead of rewriting
        (let ([token-BOF
               (token 'BOF)
               #;
               (srcloc-token (token-struct 'BOF #f #f #f #f #f #f)
                             ; srcloc will not accept 0 for column
                             (srcloc 'string #f #f #f 1))]) 
          (set! bof #f)
          #;
          (pretty-print token-BOF)
          token-BOF)
        (let ([out (laundry-lexer port)])
          #;
          (pretty-print out)
          out)))
  next-token)

(module+ test

  (define (dotest str)
    (let ([next-token (laundry-make-tokenizer (open-input-string str))])
      (next-token)
      (next-token)))

  (dotest " =hello= ")
  (dotest "=hello= ")
  (dotest "=hello=")

  )
