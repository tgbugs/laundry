#lang racket/base
(require
 (only-in racket/list splitf-at)
 (for-meta 2
           racket/base
           ;syntax/parse
           )
 racket/pretty
 (only-in laundry/tokenizer paragraph-make-tokenizer)
 (rename-in (only-in laundry/paragraph parse parse-to-datum)
            [parse parse-paragraph]
            [parse-to-datum parse-paragraph-to-datum]
            )
 (for-syntax racket/base
             racket/stxparam
             racket/pretty
             syntax/parse
             #;
             (only-in racket/list splitf-at)
             
             ))
(provide
 (rename-out [org-module-begin #%module-begin])
 (except-out (all-from-out racket/base) #%module-begin)
 (except-out (all-defined-out) org-module-begin))

(define-syntax (org-module-begin stx)
  (syntax-parse stx
    ([_ ast]
     #:do [(pretty-write (syntax->datum #'ast))]
     #'(#%module-begin
        (provide root)
        (define root ast)
        ; apparently the root binding can only be accessed inside here
        ; but not if the module is executed directly? not sure if this
        ; is a racket-mode issue or what?
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
    ([_ name]
     #:with elipsis (datum->syntax this-syntax '...)
     #'(define-syntax (name stx)
         (syntax-parse stx
           [(_ body elipsis)
            #'(string-append body elipsis)])))))

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
  parl-indent
  parl-wsnn
  parl-prp-bt
  not-prp-newline1
  parl-ws-bt-l-s
  stuff
  wordhyus
  word-char-n
  blank-line
  not-colon-newline
  not-colon-whitespace1

  big-tokes-less-d-s
  big-tokes-less-d-s-p
  big-tokes-less-d-s-p-cnt
  big-tokes-less-d-s-p-cnt-blk
  big-tokes-less-d-s-blk
  bt-chars-no-title-start
  bt-colon
  )

(define-nodes
  org-file
  org-node
  comment-element
  comment-line
  headline-node
  headline
  plain-list-line
  pl-indent
  ordered-list-line
  bullet-counter
  digits
  drawer
  end ; end is tricky because we do want to warn on it
  table
  table-row
  hyperlink

  property-drawer
  pdrawer-unparsed ; FIXME naming ??

  keyword
  keyword-line
  keyword-key
  keyword-value

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

  footnote-definition
  footnote-definition-line
  fn-label
  fn-def

  blk-src

  malformed-wsnn
  detached-block-node
  blk-greater-malformed
  planning-dissociated
  ak-key-no-colon
  babel-call-no-colon
  )

(define (newline . rest) "\n")
(define (space . rest) " ")
(define (tab . rest) "\t")

;asdf
(begin-for-syntax
  (define-syntax-parameter newline
    (lambda (stx) 'newline))
  (define (par-nl . rest)
    "\n")
  (define-syntax-parameter end
    (lambda (stx) 'end))
  (define (par-end end)
    end)

  #|
  (define-syntax-parameter stars
  (lambda (stx) 'stars))
  (define (par-stars stars)
  stars)
  |#


  )

(define-syntax (markup stx)
  (syntax-parse stx
    ([_ (type text:str)]
     #:do [(define dat-text (syntax-e #'text))
           #;
           (println dat-text)]
     #:with leading (datum->syntax #'text (substring dat-text 0 1))
     #:with text-clean (datum->syntax #'text (substring dat-text 2 (sub1 (string-length dat-text))))
     #'(unquote-splicing (list leading (type text-clean))))))

(define-syntax (paragraph stx)
  #;
  (println (list (syntax-line stx)
                 (syntax-column stx)
                 (syntax-span stx)))
  (syntax-parse stx
    ([_ body ...]
     ; won't work
     ;#:with processed (datum->syntax this-syntax (merge-paragraph (syntax->datum #'(body ...))))
     ;#:do [(pretty-write (cons 'aaaaaaaaaaaaa (syntax->datum #'processed)))]
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
       #'(cons 'paragraph (merge-paragraph (list body ...)))
       #;
       #'(quasiquote (list paragraph ,@(merge-paragraph (list body ...))))
       #;
       (let ([thing (list body ...)])
         (for/fold [] []))
       #; ; can't just sa paragraphs because they do have markup etc
       #'(cons 'paragraph (string-append body ...))))))

(define (do-paragraph str)
  ; FIXME this is gonna be a bit of work
  ; we use -to-datum here because there are two different paragraphs
  ; FIXME this is a bit busted maybe?
  (let ([out (cdr (parse-paragraph-to-datum (paragraph-make-tokenizer (open-input-string str))))])
    (pretty-write out)
    out))

(define (merge-paragraph l)
  (if (null? l)
      l
      (let ([f (car l)])
        (if (string? f)
            (let-values ([(a b) (splitf-at l string?)])
              (cons (do-paragraph (apply string-append a)) (merge-paragraph b)))
            (let-values ([(a b) (splitf-at l (compose not string?))])
              (append a (merge-paragraph b)))))))

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
