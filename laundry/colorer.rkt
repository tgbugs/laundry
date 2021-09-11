#lang racket/base

(require brag/support
         "lex-abbrev.rkt"
         (only-in "tokenizer.rkt" #;find-last get-block-type set-port-next-location-from)
         (only-in racket/port peeking-input-port)
         (only-in racket/string string-split))

(provide colorer)

(define todo-keywords (make-parameter '("TODO" "DONE")))

(define (todo-keyword-line? line)
  ; TODO factor out so that this is shared with the bit in the expander
  (when (regexp-match #rx"^#\\+(todo|TODO):" line)
    (let* ([groups (regexp-split #px"\\s+" (regexp-replace #px"(^\\s+|\\s+$)" "" (substring line 7)))]
           [new-todo-keywords (remove "|" groups)]
           [regroups (reverse groups)]
           [_ (println (list 'todo-list-groups groups))]
           [dones? (member "|" groups)] ; FIXME use a grammar so we can error on multiple pipes
           [dones (if dones? (cdr dones?) (list (car regroups)))]
           [todos (reverse (if dones? (cdr (member "|" regroups)) (cdr regroups)))])
      (todo-keywords (append new-todo-keywords (todo-keywords)))
      (void))))

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

(define (composite-category->category composite-category top-mode)
  (case composite-category
    ((export-snip) 'comment)
    ((paragraph) 'other)
    ;((src-block unknown-block) 'org-block-begin-line)
    ((org-drawer-start) (if (eq? top-mode 'drawer) 'org-malformed composite-category))
    (else composite-category)))

#;
(log-error "org colorer --------------------------------")
#;
(define (proc-comp lexeme composite-category))


(define (lex-composite input-port lexeme composite-category paren start-pos end-pos mode)
  (let* (;[lexeme 'lol]
        ;[paren #f]
        ;[start-pos 0]
        ;[end-pos 1]
        [backup-distance 0]
        #;
        [mode 'normal]
        [top-mode (if (null? mode) #f (car mode))]
        [category (composite-category->category composite-category top-mode)]
        ;[input-port (open-input-string lexeme)]
        [peek-port (peeking-input-port input-port #:init-position (+ 1 (file-position input-port)))]
        #;
        [next (case composite-category
                ((org-drawer)
                 (proc-drawer peek-port
                  lexeme composite-category paren start-pos end-pos mode
                  )
                 #; ; asdf
                 (proc-drawer lexeme start-pos end-pos))
                (else
                 (list (list lexeme category paren start-pos end-pos backup-distance mode)))
                )]
        )
    (let-values ([(line col pos) (port-next-location input-port)])
      (when line
        (port-count-lines! peek-port)))
    (set-port-next-location-from input-port peek-port)
    (file-stream-buffer-mode peek-port 'none)
    #;
    (log-error "lc cat: ~a" category)
    (case category
      [(heading-line)
       (proc-heading peek-port
                     lexeme composite-category paren start-pos end-pos backup-distance mode)
       ]
     [(org-drawer-start)
      (proc-drawer peek-port
                   lexeme composite-category paren start-pos end-pos backup-distance mode)]
     [(src-block-line-begin unknown-block-line-begin)
      (proc-block peek-port
                  lexeme composite-category paren start-pos end-pos backup-distance mode)]
     [else
       (values lexeme category paren start-pos end-pos backup-distance mode)]
      )
    #; ; if we handle category shuffling above we can use case
    (cond
      [(and (eq? category 'org-drawer-start) #;(not (eq? top-mode 'drawer)))
       (proc-drawer peek-port
                    lexeme composite-category paren start-pos end-pos backup-distance mode)
       #; ; asdf
       (proc-drawer lexeme start-pos end-pos)]
      [else
       (values lexeme category paren start-pos end-pos backup-distance mode)])
    #;
    (values
     (car next)
     (cdr next))))


(define (proc-heading peek-port
                      lexeme composite-category paren start-pos end-pos backup-distance mode)
  (let ([cycle (heading-cycle lexeme)])
    (values lexeme cycle paren start-pos end-pos backup-distance
            (cons (cons 'heading cycle) mode))))

(define (proc-drawer peek-port
                     lexeme composite-category paren start-pos end-pos backup-distance mode
                  )
  ; ah I see, the mode acts as the stack in a pda
  (define dlex
    (lexer [(from/stop-before
             "\n"
             ; I think these have to go in here because this is
             ; non-greedy whereas if we were to split into two lexer
             ; statements it is greedy
             (:or stop-before-heading
                  drawer-end-line))
            lexeme]))

  (define not-in-drawer (or (null? mode) (not (eq? (caar mode) 'drawer))))
  ; TODO if we're in a properpty drawer then these are keys, but the
  ; grammar is more complex so skip for now
  (define condition (and not-in-drawer (not (regexp-match #rx"\n[*]+$" (dlex peek-port)))))
  (define category (if condition 'org-drawer 'org-malformed))
  (define fsbm (file-stream-buffer-mode peek-port))
  #;
  (log-error "fsbm: ~a condition: ~a mode: ~a" fsbm condition mode)
  (values lexeme category paren start-pos end-pos backup-distance
          (if condition (cons (cons 'drawer #f) mode) mode))

  )

(define (proc-block
         peek-port
         lexeme composite-category paren start-pos end-pos backup-distance mode)
  ; XXX strickly speaking nested blocks do not having meaning within org, however
  ; we have to take this approach so that we can stop at the correct #+end_ in the
  ; event that a block happens to contain other #+end_ lines, this gets us to the same
  ; behavior as elisp
  (define block-type (get-block-type lexeme))
  ;(eval-syntax #`(define-lex-abbrev block-type #,block-type-name) (namespace-anchor->namespace))
  (log-error "block-type: ~a" block-type)
  (define (make-lexer)
    (eval-syntax
     #`(lexer [(from/stop-before
                "\n"
                (:or stop-before-heading
                     (:seq "\n" (:* " " "\t") (:or "#+end_" "#+END_") #,block-type (:* " " "\t"))
                     ))
               lexeme])))
  (define blex (make-lexer))

  (define condition (not (regexp-match #rx"\n[*]+$" (blex peek-port))))
  (define category (if condition 'org-block-begin-line 'org-malformed))
  (define fsbm (file-stream-buffer-mode peek-port))
  #;
  (log-error "fsbm: ~a condition: ~a mode: ~a" fsbm condition mode)
  (values lexeme category paren start-pos end-pos backup-distance
          (if condition (cons (cons 'block block-type) mode) mode))
  )



#;
(define (proc-drawer parent-lexeme parent-start-pos parent-end-pos [bu 0] [mo #f #;'dont-stop])
  ;; XXX old version where we parsed the whole thing which is incorrect
  (let-values ([(lex-drawer)
                (lexer [drawer-start-line
                        (let*-values ([(s e) (values (pos lexeme-start) (pos lexeme-end))]
                                      [(sf ef) (values (+ parent-start-pos s) (+ parent-start-pos e))])
                          (log-error "psp: ~a pep: ~a lexeme: \"~a\" start: ~a end: ~a sf: ~a ef: ~a"
                                     parent-start-pos parent-end-pos lexeme start-pos end-pos sf ef)
                          (list lexeme 'org-drawer #f
                                sf
                                ef
                                bu mo))])]
               [(trunc stop-raw) (find-last #\newline parent-lexeme)])
    (let ([out null]
          [input-port (open-input-string trunc)]
          [sl (string-length parent-lexeme)]
          [stop (+ 2 stop-raw)])
      (set! out (cons (lex-drawer input-port) out)) ; FIXME ick

      ; TODO call repeatedly I think?
      (define (nt)
        (set! ; FIXME ick
         out
         (cons
          (let-values ([(lexeme category paren start-pos end-pos backup-distance mode)
                        (colorer input-port parent-start-pos mo)])
            (log-error "lex: ~a cat: ~a sp: ~a ep: ~a" lexeme category start-pos end-pos)
            (list lexeme category paren start-pos end-pos backup-distance mode))
          out)))

      (let loop ([_ (nt)])
        (unless (eq? (peek-char input-port) eof)
          (loop (nt))))

      (let* ([end (substring parent-lexeme stop sl)]
             [end-start-pos (+ parent-start-pos stop) #;(- sl stop)]
             ; TODO check this, input-port, and parent-end-pos for consistency
             [end-end-pos (+ parent-start-pos sl)])
        (log-error "sl: ~a stop: ~a end-start: ~a end-end: ~a" sl stop end-start-pos end-end-pos)
        (set!
         out
         (cons
          (list end
                'org-drawer
                #f
                end-start-pos
                end-end-pos
                bu
                mo)
          out))) ; FIXME ick
      (reverse out))))

(define laundry-color-lexer
  ; this is a line oriented lexer that can't capture all nuance
  (lexer
   [(eof) (values lexeme 'eof #f #f #f #f)]
   #; ; not the issue
   [(from/stop-before "\n#lang org" "\n")
    (values lexeme 'comment #f #f (pos lexeme-start) (pos lexeme-end))]
   [heading ; TODO look into how to chain lexers for this
    #; ; TODO this is the solution, but we need the heading lexer to make it work
    (:& (:seq "\n" (:+ "*"))
     (from/stop-before ""
      stop-before-heading))
    (values lexeme 'heading-line  'comp #f (pos lexeme-start) (pos lexeme-end))]
   [comment-element
    (values lexeme 'comment #f #f (pos lexeme-start) (pos lexeme-end))]
   [plain-list-start
    (values lexeme 'org-bullet #f #f (pos lexeme-start) (pos lexeme-end))]
   #;
   [(:or (:seq "\n" plain-list-start)
         (:seq "\n"
               (:* (:or " " "\t"))
               ; TODO " * variant" requires :+ on the leading whitespace
               (:or "-")
               (:+ (:or " " "\t")) ))
    (values lexeme 'org-bullet #f #f (pos lexeme-start) (pos lexeme-end))]
   #;
   [(:seq "\n" (:* (:or " " "\t"))
          (:or
           (:seq (:or (:+ A-Z) (:+ a-z) (:+ 0-9)) bullet-marker)
           ; TODO " * variant" requires :+ on the leading whitespace
           (:seq (:or "-"))))
    (if (eq? (peek-char input-port) #\newline) ; ugh it hurts me that we have to do this in here
        (values lexeme 'org-bullet #f #f (pos lexeme-start) (pos lexeme-end))
        (values lexeme 'other #f #f (pos lexeme-start) (pos lexeme-end)))]
   [(from/stop-before (:seq (:* (:or " " "\t")) "|") "\n")
    ; table rows
    (values lexeme 'org-table 'comp #f (pos lexeme-start) (pos lexeme-end))]
   [(:or hyperlink hyperlink-ab)
    (values lexeme 'org-link  'comp #f (pos lexeme-start) (pos lexeme-end))]
   [drawer-end-line
    #;
    (values lexeme 'org-drawer #f #f (pos lexeme-start) (pos lexeme-end))
    (values lexeme 'org-drawer-end      #f #f (pos lexeme-start) (pos lexeme-end))]
   [drawer-start-line ; XXX only match the start on composites so we can peek forward
    ;drawer-ish
    (values lexeme 'org-drawer-start 'comp #f (pos lexeme-start) (pos lexeme-end))
    #;(:or drawer-start-line drawer-end-line)
    #;
    (if (eq? (peek-char input-port) #\newline)
        (values lexeme 'org-drawer #f #f (pos lexeme-start) (pos lexeme-end))
        (values lexeme 'error #f #f (pos lexeme-start) (pos lexeme-end)))]
   #; ; not right
   [(:seq (from/stop-before (:seq (:* (:or " " "\t")) ":" (:~ (:or ":" "\n")))
                            (:seq ":" (:* (:or " " "\t")) "\n")) ":")
    ; putative drawer bits
    (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   [keyword-element ; affiliated keywords and friends XXX broken for #+begin_:
    (begin
      (todo-keyword-line? lexeme)
      (values lexeme 'org-meta-line 'comp #f (pos lexeme-start) (pos lexeme-end)))]
   [keyword-element-malformed
    (values lexeme 'org-malformed #f #f (pos lexeme-start) (pos lexeme-end))]
   [paragraph-2
    (begin
      #;
      (log-error "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAa")
      (values lexeme 'paragraph     'comp #f (pos lexeme-start) (pos lexeme-end)))]
   #; ; nested in the paragraph (aka may contain objects) parser
   [export-snip
    (values lexeme 'export-snip 'comp #f (pos lexeme-start) (pos lexeme-end))]
   #; ; overruns and lexeme-start/lexeme-end need to be adjusted, not just file-position
   [paragraph
    (begin
      (token-stop-for-paragraph 'PARAGRAPH lexeme input-port start-pos) ; XXX not sure we want this in this context who knows
      (values lexeme 'other #f (pos lexeme-start) (pos lexeme-end)))]
   #; ; old pattern
   [(:or src-block)
    (values lexeme 'src-block 'comp #f (pos lexeme-start) (pos lexeme-end))]
   #; ; old pattern
   [(:or unknown-block)
    (values lexeme 'unknown-block 'comp #f (pos lexeme-start) (pos lexeme-end))]
   [(:or src-block-line-begin unknown-block-line-begin)
    (values lexeme 'org-block-begin-line 'comp #f (pos lexeme-start) (pos lexeme-end))]
   [(:or src-block-line-end unknown-block-line-end)
    (values lexeme 'org-block-end-line #f #f (pos lexeme-start) (pos lexeme-end))]
   [stop-before-footnote-def ; FIXME switch to composite here
    (values lexeme 'org-footnote 'comp #f (pos lexeme-start) (pos lexeme-end))]
   [any-char
    (values lexeme 'other #f #f (pos lexeme-start) (pos lexeme-end))]))

; https://docs.racket-lang.org/framework/Color.html see start-colorer get-token
(define colorer
  (let ([token-stack null])
    (define (get-token input-port offset dont-stop-thing)

      ; we can do lookahead in this by not calling laundry-color-lexer
      ; if there are still tokens on the stack

      ; I do have this sense that I am reinventing some machinery here
      ; because the documentation alludes to lexers reading ahead to
      ; determine the token at a given point, but I have been unable
      ; to get ... hrm looking at module-lexer it seems that
      ; set-port-next-location! has to be used for this? but they also wrap
      ; the input port in peeking-input-port

      ; sub-tokens from composite tokens need to return a backup
      ; distance that takes them back to the start of the composite
      ; token so that the whole block can be recolored, this is
      ; unfortunate for long code blocks, so maybe it isn't actually
      ; necessary in all cases?

      ; TODO a more complex colorer is required to even get close to org highlighting
      ; best case it searches backward to the previous heading in reasonably
      ; sized chunks, and then once found it parses that section to the next
      ; heading, and probably uses a cache of the parse tree to keep it fast

      #; ; TODO heading lexer
      (and (not (null? mode))
           (eq? (caar mode) 'heading))

      (if (null? token-stack)
          (let ([lexeme 'lol]
                [category 'other]
                [paren #f]
                ;[start-pos 0]
                ;[end-pos 1]
                [backup-distance 0]
                [mode (or dont-stop-thing '())])
            (let*-values ([(lexeme category composite paren start-pos end-pos)
                           (laundry-color-lexer input-port)]
                          #;
                          [(start-pos-corr end-pos-corr)
                           (values (+ offset start-pos-raw) (+ offset end-pos-raw))])
              #;
              (log-error "os: ~a sp: ~a ep: ~a fp: ~a ip: ~a lx: ~a"
                         offset start-pos end-pos (file-position input-port) input-port lexeme)
              #;
              (log-error "colorer mode: ~a lexeme: ~a" mode lexeme)
              (if composite
                  (lex-composite
                   input-port
                   lexeme category paren
                   start-pos
                   end-pos
                   ;(if (> offset 0) (+ offset start-pos) start-pos)
                   ;(if (> offset 0) (+ offset end-pos) end-pos)
                   mode)
                  #;
                  (let-values ([(next next-token-stack)
                                (lex-composite
                                 input-port
                                 lexeme category paren start-pos end-pos mode
                                 )])
                    (unless (null? next-token-stack)
                      (set! token-stack next-token-stack))
                    (apply values next))
                  (if (and (not (null? mode))
                           (eq? (caar mode) 'drawer)
                           (eq? category 'org-drawer-end))
                      (begin
                        #;
                        (log-error "FFFFFFFFFFFF")
                        (values
                        lexeme
                        'org-drawer
                        paren
                        start-pos
                        end-pos
                        ;(if (> offset 0) (+ offset start-pos) start-pos)
                        ;(if (> offset 0) (+ offset end-pos) end-pos)
                        backup-distance ; FIXME I think we need to store the backup distance
                        (cdr mode)
                        ))
                      (cond
                        [(and (not (null? mode))
                              (eq? (caar mode) 'block)
                              (eq? category 'org-block-end-line)
                              (string=? (get-block-type lexeme) (cdar mode))
                              )
                         ; FIXME return value ??
                         ]
                        #; ; FIXME this is handled earlier?
                        [(and (not (null? mode))
                              (eq? (caar mode) 'heading))]
                        [else
                         (values
                          lexeme
                          category
                          paren
                          start-pos
                          end-pos
                          ;(if (> offset 0) (+ offset start-pos) start-pos)
                          ;(if (> offset 0) (+ offset end-pos) end-pos)
                          backup-distance
                          mode)])))))
          (error "we don't use this approach anymore")
          #;
          (let ([next (car token-stack)])
            (set! token-stack (cdr token-stack))
            (apply values next))
          ))
    get-token))

(module+ test
  (require framework)
  color:text% ; apparently hard to find the -text<%> interface due to weird renamings
  (define test-string "* hello there\nsome text\n** hrm\nmore text\n")
  (define sigh (open-input-string test-string))
  )
