#lang racket/base

(require brag/support
         "lex-abbrev.rkt"
         (only-in racket/string string-split))

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

(define (heading-cycle lexeme)
  (let ([level (string-length (car (string-split lexeme " " #:trim? #t)))]
        [cycle-length 9])
    #;
    (log-error "lexeme: ~a heading level: ~a cycle: ~a" lexeme level cycle-length)
    (string->symbol (format "org-level-~a" (add1 (modulo (- level 2) cycle-length))))))

(define laundry-color-lexer
  ; this is a line oriented lexer that can't capture all nuance
  (lexer
   [(eof) (values lexeme 'eof #f #f #f)]
   [heading ; TODO look into how to chain lexers for this
    (values lexeme (heading-cycle lexeme) #f (pos lexeme-start) (pos lexeme-end))]
   [comment-element
    (values lexeme 'comment #f (pos lexeme-start) (pos lexeme-end))]
   [(:or (:seq "\n" plain-list-start)
         (:seq "\n"
               (:* (:or " " "\t"))
               ; TODO " * variant" requires :+ on the leading whitespace
               (:or "-")
               (:+ (:or " " "\t")) ))
    (values lexeme 'org-bullet #f (pos lexeme-start) (pos lexeme-end))]
   [(:seq "\n" (:* (:or " " "\t"))
          (:or
           (:seq (:or (:+ A-Z) (:+ a-z) (:+ 0-9)) bullet-marker)
           ; TODO " * variant" requires :+ on the leading whitespace
           (:seq (:or "-"))))
    (if (eq? (peek-char input-port) #\newline) ; ugh it hurts me that we have to do this in here
        (values lexeme 'org-bullet #f (pos lexeme-start) (pos lexeme-end))
        (values lexeme 'other #f (pos lexeme-start) (pos lexeme-end)))]
   [(from/stop-before (:seq (:* (:or " " "\t")) "|") "\n")
    ; table rows
    (values lexeme 'org-table #f (pos lexeme-start) (pos lexeme-end))]
   [(:or hyperlink hyperlink-ab)
    (values lexeme 'org-link #f (pos lexeme-start) (pos lexeme-end))]
   [(:or drawer-start-line drawer-end-line)
    (if (eq? (peek-char input-port) #\newline)
        (values lexeme 'org-drawer #f (pos lexeme-start) (pos lexeme-end))
        (values lexeme 'other #f (pos lexeme-start) (pos lexeme-end)))]
   #; ; not right
   [(:seq (from/stop-before (:seq (:* (:or " " "\t")) ":" (:~ (:or ":" "\n")))
                            (:seq ":" (:* (:or " " "\t")) "\n")) ":")
    ; putative drawer bits
    (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   [keyword-element ; affiliated keywords and friends XXX broken for #+begin_:
    (values lexeme 'org-meta-line #f (pos lexeme-start) (pos lexeme-end))]
   [paragraph-2 (values lexeme 'other #f (pos lexeme-start) (pos lexeme-end))]
   [(from/to "@@comment:" "@@") ; FIXME line limit issues
    (values lexeme 'comment #f (pos lexeme-start) (pos lexeme-end))]
   #; ; overruns and lexeme-start/lexeme-end need to be adjusted, not just file-position
   [paragraph
    (begin
      (token-stop-for-paragraph 'PARAGRAPH lexeme input-port start-pos) ; XXX not sure we want this in this context who knows
      (values lexeme 'other #f (pos lexeme-start) (pos lexeme-end)))]
   [(:or src-block-line-begin unknown-block-line-begin)
    (values lexeme 'org-block-begin-line #f (pos lexeme-start) (pos lexeme-end))]
   [(:or src-block-line-end unknown-block-line-end)
    (values lexeme 'org-block-end-line #f (pos lexeme-start) (pos lexeme-end))]
   [stop-before-footnote-def
    (values lexeme 'org-footnote #f (pos lexeme-start) (pos lexeme-end))]
   [any-char
    (values lexeme 'other #f (pos lexeme-start) (pos lexeme-end))]))

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
