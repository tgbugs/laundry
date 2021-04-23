#lang racket/base
(require
 (only-in racket/function identity)
 (only-in racket/list splitf-at)
 (for-meta 2
           racket/base
           syntax/parse
           )
 racket/pretty
 (only-in laundry/parser make-rule-parser)
 ;(only-in laundry/tokenizer ) ; FIXME -> syntax time
 (for-syntax (only-in laundry/tokenizer paragraph-make-tokenizer bind-runtime-todo-keywords))
 (for-syntax (rename-in (only-in laundry/heading parse parse-to-datum)
                        [parse parse-heading]
                        [parse-to-datum parse-heading-to-datum])
             (rename-in (only-in laundry/paragraph parse parse-to-datum)
                        [parse parse-paragraph]
                        [parse-to-datum parse-paragraph-to-datum]))
 (for-syntax racket/base
             racket/stxparam
             racket/pretty
             syntax/parse
             (only-in racket/string string-trim string-split)
             #;
             (only-in racket/list splitf-at)
             
             ))
(provide
 (rename-out [org-module-begin #%module-begin])
 (except-out (all-from-out racket/base) #%module-begin)
 (except-out (all-defined-out) org-module-begin))

; FIXME someone let me bind this stuff at runtime and then
; parameterize it later basically I want to be able to parse the first
; section of the file, bind the runtime values, and then parse the
; rest of the syntax, I probably have to do that with a syntax local
; variable or something like that though to make the macros cooperate
; rather than trying to pass it in
; XXX possibly via syntax-local-value or something? so that #+todo: statements expand the syntax value?
(define-for-syntax runtime-todo-keywords (make-parameter '("TODO" "DEFAULT"))) ; FIXME centralize?

(define-syntax (org-module-begin stx)
  (syntax-parse stx
    ([_ ast]
     #:do [(pretty-write `(org-module-begin: ,(syntax->datum #'ast)))]
     #'(#%module-begin
        (provide root)
        (define root ast)
        ; apparently the root binding can only be accessed inside here
        ; but not if the module is executed directly? not sure if this
        ; is a racket-mode issue or what?
        (require racket/pretty)
        (pretty-write root)
        (module+ main
          root)))))

#;
(define-syntax (org-file stx)
  (syntax-parse stx
    ([_ body ...]
     #'(define root (cons 'org-file body ...))
     #;
     #'(define root (quote (org-file body ...))))))

(define-syntax (define-node stx)
  (syntax-parse stx
    ([_ name]
     #:with elipsis (datum->syntax this-syntax '...)
     #'(define-syntax (name stx)
         (syntax-parse stx
           [(_ body elipsis)
            #'(cons 'name (list body elipsis))])))))

(define-syntax (define-nodes stx)
  (syntax-parse stx
    [(_ name ...)
     #'(begin (define-node name) ...)]))

(define-syntax (define-sa-node stx)
  (syntax-parse stx
    [(_ name)
     #:with elipsis (datum->syntax this-syntax '...)
     #:with elipsis-plus (datum->syntax this-syntax '...+)
     #'(define-syntax (name stx)
         (syntax-parse stx
           [(_) #'""]
           [(_ body)
            (local-expand #'body 'expression #f)]
           [(_ body elipsis-plus)
            #:with appended (datum->syntax
                             #'(body elipsis)
                             (apply string-append
                                    (map (λ (e) (syntax->datum (local-expand e 'expression #f)))
                                         (syntax-e #'(body elipsis)))))
            #'appended]))]))

(define-syntax (define-sa-nodes stx)
  (syntax-parse stx
    [(_ name ...)
     #'(begin (define-sa-node name) ...)]))

(define-syntax (define-sa-keep-node stx)
  (syntax-parse stx
    ([_ name]
     #:with elipsis (datum->syntax this-syntax '...)
     #'(define-syntax (name stx)
         (syntax-parse stx
           [(_ body elipsis)
            #'(list 'name (string-append body elipsis))])))))

(define-syntax (define-sa-keep-nodes stx)
  (syntax-parse stx
    [(_ name ...)
     #'(begin (define-sa-keep-node name) ...)]))

(define-sa-keep-nodes
  h-title
  plain-list-line-tail
  table-cell)

(define-sa-nodes ; XXX things in this list should probably be spliced out
  parl-start
  not-pl-start-whitespace1
  ns-nwt-less-negated
  not-newline
  not-whitespace
  stars
  alpha-n
  alphas
  alphas-unmixed ; only the parser needs to know that they are unmixed

  headline-content ; this is sa because we have to remerge the whole headline for the 2nd pass parser

  ; put the headline titles here for now until we can figure out what
  ; to do about the markup for titles
  ; all spliced out now
  ;h-title-r-p-c
  ;h-title-p-c
  ;h-title-c
  ;;h-title

  parl-indent
  parl-wsnn
  parl-prp-bt
  parl-ncn-bt-l-d
  not-prp-newline1
  parl-ws-bt-l-s
  parl-ws-bt
  parl-pw-bt

  stuff
  wordhyus
  word-char-n
  blank-line
  not-alpha-newline1
  not-colon-newline
  not-colon-whitespace1
  not-colon-newline1
  not-plus-whitespace1
  not-whitespace-l-d
  not-whitespace1
  not-asterisk-whitespace1

  big-tokes-less-d-s
  big-tokes-less-d-s-p
  big-tokes-less-d-s-p-cnt
  big-tokes-less-d-s-p-cnt-blk
  big-tokes-less-d-s-blk
  bt-chars-no-title-start
  bt-chars-plan
  bt-chars
  bt-colon
  bt-hash
  )

(define-nodes
  org-file
  org-node
  org-node-dyn
  comment-element
  comment-line

  headline-node
  ;headline
  ;heading ; FIXME we need to deal with the number of stars here
  heading-content
  todo-keyword
  h-priority
  h-comment
  ;h-tags
  archive

  plain-list-line
  descriptive-list-line
  ordered-list-line
  pl-indent
  bullet-counter
  bullet-plain
  drawer
  drawer-name
  end ; end is tricky because we do want to warn on it
  table
  table-row
  hyperlink

  bof
  digits
  nlpws ; this can't be saed because it contains an indent that is needed in some cases?

  property-drawer
  pdrawer-unparsed ; FIXME naming ??

  keyword
  keyword-line
  keyword-key
  keyword-value
  keyword-key-sigh
  keyword-value-sigh

  affiliated-keyword
  un-affiliated-keyword
  name
  header
  plot
  ak-key
  ak-value
  ak-key-attr
  attr-backend
  ak-opt
  ak-key-opt
  ak-key-name-opt

  babel-call

  footnote-definition
  footnote-definition-line
  fn-label
  fn-def

  blk-src
  blk-src-begin
  blk-src-line-contents
  language
  blk-src-line-rest-alt
  switches-sane          --test--switches-sane
  switch-sane
  format-string
  format-string-contents
  blk-src-args-broken
  blk-src-parameters
  blk-src-contents
  blk-src-end
  block-begin-line
  block-end-line

  blk-greater-begin
  blk-greater-type
  block-type-name
  bg-type-special
  block-type-rest
  blk-greater-end
  bg-end-special

  blk-dyn
  blk-dyn-begin
  blk-line-contents
  blk-dyn-contents
  blk-dyn-end

  malformed
  malformed-wsnn
  detached-block-node
  blk-greater-malformed
  det-blk-src-begin
  det-blk-src-end
  planning-dissociated
  ak-key-no-colon
  babel-call-no-colon
  )

(define-syntax (newline stx) (syntax/loc stx "\n"))
(define-syntax (space stx) (syntax/loc stx " "))

#;
(define (newline . rest) "\n")
#;
(define (space . rest) " ")
(define (tab . rest) "\t")

(define-syntax (blank stx)
  (syntax-parse stx
    ([_ thing:str]
     #'thing)))

; heading
(begin-for-syntax
  (define-syntax-parameter newline
    ; XXX we're going to have to switch this so that the org-node newline case can be handled?
    (lambda (stx) 'newline))
  #; ; par-nl needs to be syntax so expansion can happen at compile time
  (define (par-nl . rest)
    "\n")
  (define-syntax (par-nl stx)
    (syntax-parse stx
      [(_ ignore)
       #'"\n"]))
  (define-syntax-parameter end
    (lambda (stx) 'end))
  (define (par-end end)
    end)

  (define-syntax-parameter stars
    (lambda (stx) 'stars))

  )

#;
(define-syntax (heading-stars stx)
  (syntax-parse stx
    [(_ stars)
     (datum->syntax
      #'stars
      (string-length
       (string-trim
        (syntax->datum
         #'stars))))]))

(define-syntax (headline stx)
  (syntax-parse stx
    [(_ body ...)
     ;#:do [(define int-ctx (syntax-local-make-definition-context))]
     (datum->syntax
      #'(body ...)
      (do-headline
       (apply string-append
              (map (λ (e)
                     (syntax->datum
                      (local-expand e 'expression #f
                                    #; ; shouldn't need because we use datum->syntax above
                                    (internal-definition-context-track
                                     int-ctx
                                     #'(body ...))
                                    )))
                   (syntax-e #'(body ...))))))]))

(define-syntax (heading stx)
  (syntax-parse stx
    #:literals (stars)
    [(_ (stars sstring) pct ... tags)
     #:with depth (datum->syntax
                   #'sstring
                   (string-length
                    (string-trim
                     (syntax->datum
                      #'sstring))))
     #:with (other ...)
     ; TODO priority comment title
     #'(pct ...)
     #:with tags-list (let ([t (syntax->datum #'tags)])
                        (if t
                            (local-expand #'tags 'expression #f)
                            #''()))
     #'(cons 'heading (list depth other ... tags-list))])
  #;
  (syntax-parse stx
    ([_ stars content tags]
     (local-expand )
     #'(heading depth content tag-list)
     )))

(define-for-syntax (process-tag-string tag-string)
  (datum->syntax
   tag-string
   (cons 'list
         (string-split
          (string-trim
           (syntax->datum tag-string)
           #px"\\s+|:") ":"))))

(define-syntax (h-tags stx)
  (syntax-parse stx
    [(_ tag-string:str)
     (process-tag-string #'tag-string)]))

(define-syntax h-t-rpc-t (make-rename-transformer #'h-title))
(define-syntax h-t-pc-t (make-rename-transformer #'h-title))
(define-syntax h-t-c-t (make-rename-transformer #'h-title))

(define-for-syntax (with-newlines str)
  (let ([prepended (if (eq? (string-ref str 0) #\newline)
                       str
                       (string-append "\n" str))] )
    (if (eq? (string-ref str (sub1 (string-length str))) #\newline)
        prepended
        (string-append prepended "\n"))))

(define-for-syntax (do-headline str)
  (let* ([heading-make-tokenizer (bind-runtime-todo-keywords (runtime-todo-keywords))]
         [out (
               #;
               (compose syntax->datum (make-rule-parser headline-content-2))
               parse-heading-to-datum
               (heading-make-tokenizer
                (open-input-string (let ([s (with-newlines str)])
                                     (displayln (format "AAAAAAAAAAAAAAAA: ~s" s))
                                     s
                                     ))))])
    out))

; paragraph

(define-syntax (markup stx)
  (syntax-parse stx
    ([_ (type text:str)]
     #:do [(define dat-text (syntax-e #'text))
           #;
           (println dat-text)]
     #:with leading (datum->syntax #'text (substring dat-text 0 1))
     #:with text-clean (datum->syntax #'text (substring dat-text 2 (sub1 (string-length dat-text))))
     #'(unquote-splicing (list leading (type text-clean))))))

(define-syntax (paragraph-2 stx)
  (syntax-parse stx
    ([_ body ...]
     #:with (wat ...)
     (map (λ (e) (local-expand e 'expression (list #'markup))) (syntax-e #'(body ...)))
     #'(cons 'paragraph (list wat ...))
     )
    )
  )

(define-syntax (paragraph stx)
  #;
  (println (list (syntax-line stx)
                 (syntax-column stx)
                 (syntax-span stx)))
  (syntax-parse stx
    [(_ body ...)
     (datum->syntax
      #'(body ...)
      (do-paragraph
       (apply string-append
              (map (λ (e)
                     (syntax->datum
                      (local-expand e 'expression #f)))
                   (syntax-e #'(body ...))))))
     ; won't work
     ;#:with processed (datum->syntax this-syntax (merge-paragraph (syntax->datum #'(body ...))))
     ;#:do [(pretty-write (cons 'aaaaaaaaaaaaa (syntax->datum #'processed)))]
     #;
     (syntax-parameterize ([newline (make-rename-transformer #'par-nl)]
                           [end (make-rename-transformer #'par-end)] ; may need to flag this in some other way
                           #;
                           [stars (make-rename-transformer #'par-stars)]
                           )
       ; if all strings join
       ; if node join before cons and then join after
       ; FIXME pretty sure we need to run merge-paragraph at compile time?
       #;
       #'processed ; FIXME if we want to be able to run this at
       ; compile time I think we are going to need to require a bunch
       ; of the definitions in this file at both syntax time and at
       ; regular time
       #'(cons 'paragraph (merge-thing do-paragraph (list body ...)))
       #;
       #'(quasiquote (list paragraph ,@(merge-paragraph (list body ...))))
       #;
       (let ([thing (list body ...)])
         (for/fold [] []))
       #; ; can't just sa paragraphs because they do have markup etc
       #'(cons 'paragraph (string-append body ...)))]))

(define-for-syntax (do-paragraph str)
  ; FIXME this is gonna be a bit of work
  ; we use -to-datum here because there are two different paragraphs
  ; FIXME this is a bit busted maybe?
  (let ([out (parse-paragraph-to-datum
              (paragraph-make-tokenizer
               (open-input-string str)))])
    (pretty-write out)
    out))

(define (merge-thing do-fun l)
  (if (null? l)
      l
      (let ([f (car l)]
            ; see if the paragraph parser is causing the slowdown
            #;
            [do-fun identity])
        (if (string? f)
            (let-values ([(a b) (splitf-at l string?)])
              (cons (do-fun (apply string-append a)) (merge-thing do-fun b)))
            (let-values ([(a b) (splitf-at l (compose not string?))])
              (append a (merge-thing do-fun b)))))))

(module+ test-paragraph
  (require racket)
  (define tv '(paragraph "a" "b" (c "d") (hrm) "e" "f"))
  (define v (cdr tv))
  (define splits (indexes-where v (λ (e) (not (string? e)))))
  (splitf-at v (λ (e) (not (string? e))))
  (splitf-at v string?)
  #; ; now at syntax time
  (merge-paragraph v)

  #;
  (let ([splits (for/list ([e v]
                           [i (in-naturals)]
                           #:when (not (string? e)))
                  i)]
        )
    '(("a" "b")
      (c "d")
      (hrm)
      ("e" "f"))
    splits)
  )

;; keywords

(define-syntax (kw-prefix stx)
  ; XXX temporary implementation detail to compensate for e.g. #+title being a token
  (syntax-parse stx
    [(_ with-prefix:str)
     #:with without-prefix (datum->syntax #'with-prefix (substring (syntax-e #'with-prefix) 2))
     #'(keyword-key without-prefix)]))

(define-syntax (todo-spec-line stx)
  ; XXX TODO this is where we have to bind the runtime todo keywords
  (syntax-parse stx
    [(_ line:str)
     #'(cons 'todo-spec-line line) ; XXX TODO
     ]))
