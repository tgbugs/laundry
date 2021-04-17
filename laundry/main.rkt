#lang racket/base
(require syntax/strip-context
         org-mode/tokenizer
         org-mode/parser)
(require racket/pretty)

(define printed #t)

(define (read-syntax source-name in-port)
  ; why is this called more than once???
  (define parse-tree
    (parse source-name
           (org-mode-make-tokenizer in-port)))
  (define output-syntax
    (strip-context
     #`(module asdf org-mode/expander
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
      (case key [else default]))
    handle-query))
