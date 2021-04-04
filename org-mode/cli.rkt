#!/usr/bin/env racket
#lang racket/base

(require "test.rkt"
         (only-in racket/pretty pretty-print))

(module+ main
  (for ([path-string (current-command-line-arguments)])
    (pretty-print (dotest-file path-string #:verbose #t))))
