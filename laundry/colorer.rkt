#lang racket/base

(require brag/support
         (only-in "tokenizer.rkt"
                  comment-element
                  drawer-ish
                  heading
                  hyperlink
                  #|
                  markup-*
                  markup-/
                  markup-_
                  markup-+
                  markup-=
                  markup-~
                  |#
                  paragraph))

(provide colorer)

#;
(module+ sigh ; not sure where to put this to get it to not cause errors in drr
  (require
   (only-in racket/class make-object)
   ;(only-in racket/gui color%)
   (only-in framework color-prefs:add-color-scheme-entry))
  ; https://docs.racket-lang.org/framework/Color_Prefs.html
  ; color-prefs:add-color-scheme-entry use this to register all the org bits
  ; the string version is a color name
  (color-prefs:add-color-scheme-entry 'org-heading
                                      ;(make-object color% 0 100 0)
                                      ;(make-object color% 0 100 0)
                                      #:bold? #t)
  #;
  (color-prefs:add-color-scheme-entry 'org-keyword
                                      "" ; bow
                                      "" ; wob
                                      ))

(define laundry-color-lexer
  ; this is a line oriented lexer that can't capture all nuance
  #| we're going to need more than these types, I wonder if it is possible to extend/modify the setup ...
  'symbol
  'keyword
  'comment
  'string
  'constant
  'parenthesis
  'error
  'other
  |#
  (lexer
   [(eof) (values lexeme 'eof #f #f #f)]
   [heading (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   [(from/stop-before (:seq (:* (:or " " "\t")) "|") "\n")
    ; table rows
    (values lexeme 'constant #f (pos lexeme-start) (pos lexeme-end))]
   [hyperlink (values lexeme 'constant #f (pos lexeme-start) (pos lexeme-end))]
   [comment-element (values lexeme 'comment #f (pos lexeme-start) (pos lexeme-end))]
   [drawer-ish (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   #; ; not right
   [(:seq (from/stop-before (:seq (:* (:or " " "\t")) ":" (:~ (:or ":" "\n")))
                            (:seq ":" (:* (:or " " "\t")) "\n")) ":")
    ; putative drawer bits
    (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   [(from/to  "#+" (:seq ":" (:or " " "\t")))
    ; affiliated keywords and friends
    (values lexeme 'keyword #f (pos lexeme-start) (pos lexeme-end))]
   [any-char (values lexeme 'other #f (pos lexeme-start) (pos lexeme-end))]))

; https://docs.racket-lang.org/framework/Color.html see start-colorer get-token
(define (colorer input-port offset dont-stop-thing)
  ; TODO a more complex colorer is required to even get close to org highlighting
  ; best case it searches backward to the previous heading in reasonably
  ; sized chunks, and then once found it parses that section to the next
  ; heading, and probably uses a cache of the parse tree to keep it fast
  (let ([lexeme 'lol]
        [category 'other]
        [paren? #f]
        [start-pos 0]
        [end-pos 1]
        [backup-distance 0]
        [mode 'normal])
    (let-values ([(lexeme category paren start-pos end-pos)
                  (laundry-color-lexer input-port)])
      (values
       lexeme
       category
       paren?
       start-pos
       end-pos
       backup-distance
       mode))))

(module+ test
  (require framework)
  color:text% ; apparently hard to find the -text<%> interface due to weird renamings
  (define test-string "* hello there\nsome text\n** hrm\nmore text\n")
  (define sigh (open-input-string test-string))
  )
