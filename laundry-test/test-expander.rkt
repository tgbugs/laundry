#lang racket/base
(require
 racket/pretty
 (rename-in
  (only-in
   laundry/expander
   footnote-inline
   footnote-inline-simple
   paragraph-node
   paragraph-inline
   footnote-reference
   do-paragraph
   laundry-expander-debug)
  [laundry-expander-debug debug])
 (for-syntax
  racket/base
  racket/pretty
  #;
  (rename-in
   (only-in
    laundry/expander
    do-paragraph
    laundry-expander-debug)
   [laundry-expander-debug debug]))
         )

; test modules broken out to avoid slow recompilation

(module+ test-footnote-inline
  (footnote-inline "[fn:y:" (paragraph-inline "N."))
  (footnote-inline-simple "[fn:y:N.]")

  (footnote-inline "[fn::" (paragraph-inline "N."))
  (footnote-inline-simple "[fn::N.]")

  (paragraph-node          "N")
  (paragraph-node          "NN")
  (paragraph-node          "NN ")
  (paragraph-node          " NN ")

  (paragraph-node          "[fn::]")

  (paragraph-node          "[fn::N]")

  (paragraph-node          "[fn::N.]")

  (paragraph-node         "I[fn::N.].")

  (paragraph-node    "[fn::I[fn::N.].]")

  (paragraph-node "B [fn::I[fn::N.].] A.")

  (paragraph-node "B [fn:x:I[fn:y:N.].] A.")

  (paragraph-inline "I" (footnote-reference (footnote-inline-simple "[fn::N.]")) ".")

  (footnote-reference (footnote-inline-simple "[fn::N.]"))

  (footnote-inline-simple "[fn::N.]")

  (paragraph-inline "I" (footnote-reference (footnote-inline-simple "[fn::N.]")) ".")

  (paragraph-inline "a" "b" "c" "d" "e" "f" "g")

  )

(module+ test-paragraph-node

  ; FIXME some of these forms actually never make it through
  ; to the paragraph parser becuase the double newline
  ; is removed before we ever arrive
  (paragraph-node "[fn::\n\n")
  (paragraph-node "\n[fn::\n\n\n")

  (paragraph-node "[fn::")

  )

(module+ test-do-paragraph
  (define-for-syntax (time-it str)
    (let-values ([(out cpu real gc) (time-apply do-paragraph (list str))])
      real))

  (begin-for-syntax
    (when (debug)
      (pretty-write (time-it (make-string 10000 #\Space)))
      (pretty-write (let ([n 100]) (list n (time-it (make-string n #\[)))))
      (pretty-write (let ([n 200]) (list n (time-it (make-string n #\[)))))
      (pretty-write (let ([n 400]) (list n (time-it (make-string n #\[)))))
      (pretty-write (let ([n 800]) (list n (time-it (make-string n #\[)))))
      (pretty-write (let ([n 10]) (list n (time-it (make-string n #\{)))))
      (pretty-write (let ([n 20]) (list n (time-it (make-string n #\{)))))
      (pretty-write (let ([n 40]) (list n (time-it (make-string n #\{))))))
    )
  )

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

(module+ test-mal
  ;#; ; somehow this works at the top level but not in module scope !?
  ; WAIT !? somehow this is working now !??!?
  ; argh, it works here but not in a requiring module !?
  (paragraph-node (malformed (blk-src-begin "oops")))
  (paragraph-node "a\nb\nc\nd\ne")
  )

(module+ test
  (require
   (submod ".." test-footnote-inline)
   (submod ".." test-paragraph-node)
   (submod ".." test-do-paragraph)
   (submod ".." test-paragraph)
   (submod ".." test-mal)
   ))
