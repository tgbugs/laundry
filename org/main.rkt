#lang racket/base
(require syntax/strip-context
         laundry/tokenizer
         laundry/colorer
         laundry/grammar/parser)
(require racket/pretty)

(define printed #t)

(define (read-syntax source-name input-port)
  ; why is this called more than once???
  (define parse-tree
    (parameterize ([laundry-final-port #f])
      (parse source-name
             (laundry-make-tokenizer input-port))))
  (define output-syntax
    (strip-context
     #`(module org-module laundry/expander
         #,parse-tree)))
  (when (not printed)
    (begin (pretty-write (syntax->datum output-syntax))
           (set! printed #t)))
  output-syntax)

(module+ reader
  (provide read-syntax get-info)
  (define (get-info port sourc-module source-line
                    source-collection source-position)
    (define (handle-query key default)
      (case key
        [(color-lexer) colorer]
        [else default]))
    handle-query))
