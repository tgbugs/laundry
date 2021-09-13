#lang racket/base

(provide (all-defined-out))

(define use-sub-superscripts (make-parameter '|{}|)) ; #t #f {}
; XXX  isnt this conditional on export settings ?

(define export-with-sub-superscripts (make-parameter '|{}|)) ; #t #f {}
