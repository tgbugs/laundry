#lang racket/base

(require brag/support
         (for-syntax racket/base
                     #;
                     syntax/parse
                     (only-in racket/list combinations permutations)
                     ))
(provide (all-defined-out))

#; ;
(define-lex-abbrev whitespace (:or "\n" "\t" " ")) ; pretty sure this is complete

(define-lex-abbrev mu-pre-not-newline (:or " " "\t" "-" "(" "{" "'" "\""))
(define-lex-abbrev mu-pre-1 (:or "\n" mu-pre-not-newline))
(define-lex-abbrev mu-pre (:+ mu-pre-1))
; FIXME mu-post can also be newline, which is a problem due to newline
; first from/stop-before solves this
(define-lex-abbrev mu-post-not-newline (:or " " "\t" "-" "." "," ";" ":" "!" "?" "'" ")" "}" "[" "\""))
(define-lex-abbrev mu-post-1 (:or "\n" mu-post-not-newline))
(define-lex-abbrev mu-post (:+ mu-post-1))

(define-lex-abbrev mu-border (:~ whitespace))
(define-lex-abbrev mu-marker (:or "*" "/" "_" "+" "=" "~"))

; using from/stop-before and not worrying about the 3 line limit due to its increased complexity
; XXX the issue is that this eats any number of other things that have higher priority, such as
; headlines, so I'm 99% sure that this has to be done in a second pass that only applies to paragraphs

; FIXME TODO work out the rest of the patterns here
(define-lex-abbrev markup-= (:or
                             (:seq (from/stop-before (:seq mu-pre "=" mu-border) (:seq mu-border "=" mu-post)) mu-border "=")
                             (from/stop-before (:seq mu-pre "=" mu-border "=") mu-post)))
(define-lex-abbrev markup-~ (:or (:seq (from/stop-before (:seq mu-pre "~" mu-border) (:seq mu-border "~" mu-post)) mu-border "~")
                                 (from/stop-before (:seq mu-pre "~" mu-border "~") mu-post)))

(define-lex-abbrev markup-=-eof? (:or (:seq (from/stop-before (:seq mu-pre "=" mu-border) (:seq mu-border "=" #;mu-post)) mu-border "=")
                                      (:seq mu-pre "=" mu-border "=")))

(define-lex-abbrev markup-~-eof? (:or (:seq (from/stop-before (:seq mu-pre "~" mu-border) (:seq mu-border "~" #;mu-post)) mu-border "~")
                                      (:seq mu-pre "~" mu-border "~")))

; FIXME TODO investigate interactions between verbatim/code and other markup XXXXXXXXXX
; technically org immplements this by applying the paragraph parser repeatedly to the
; internal contents of markup, which is ... probably less than optimal to say the least

(define-syntax (make-markup-abbrevs stx)
  (let ([combs (map (λ (chars) (apply string chars))
                    (cdr (combinations (string->list "*/_+"))))])
    #`(begin #,@
        (for/list ([c combs])
          (list 'begin
                (list 'provide (string->symbol (format "markup-~a" c)))
                (list 'define-lex-abbrev
                      (string->symbol (format "markup-~a" c))
                      (cons ':or
                            (for/list ([mut (map (λ (chars) (apply string chars))
                                                 (permutations (string->list c)))])
                              (define stop (list->string (reverse (string->list mut))))
                              ; TODO check behavior to make sure cases like =* a b * c d e *= work
                              #`(:or (:seq (from/stop-before (:seq mu-pre #,mut mu-border)
                                                             (:seq mu-border #,stop mu-post))
                                           mu-border
                                           #,stop)
                                     (from/stop-before (:seq mu-pre #,mut mu-border #,stop) mu-post)))))
                (list 'provide (string->symbol (format "markup-~a-eof?" c)))
                (list 'define-lex-abbrev
                      (string->symbol (format "markup-~a-eof?" c))
                      (cons ':or
                            (for/list ([mut (map (λ (chars) (apply string chars))
                                                 (permutations (string->list c)))])
                              (define stop (list->string (reverse (string->list mut))))
                              ; TODO check behavior to make sure cases like =* a b * c d e *= work
                              #`(:or (:seq (from/stop-before (:seq mu-pre #,mut mu-border)
                                                             (:seq mu-border #,stop #;(:? mu-post)))
                                           mu-border
                                           #,stop)
                                     (:seq mu-pre #,mut mu-border #,stop))))))))))

(make-markup-abbrevs)
