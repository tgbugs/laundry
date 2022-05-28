#lang racket/base

(require laundry/colorer)

(module+ test
  (require framework)
  color:text% ; apparently hard to find the -text<%> interface due to weird renamings
  (define test-string "* hello there\nsome text\n** hrm\nmore text\n")
  (define sigh (open-input-string test-string))
  )
