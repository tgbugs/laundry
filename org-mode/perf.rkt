#lang racket/base
(require "test.rkt"
         plot
         racket/list
         racket/string)

#; ; seemingly fast
(define simple (dotest-file "simple.org"))
#; ; quaaaadraticcccccccccccc
(define notes (dotest-file "/home/tom/git/sparc-curation/docs/notes.org"))

(define (one-long n rep)
  (string-join
   (for/list ([i rep])
     (string-append (make-string n #\a) "\n"))))

(define (with-sep n rep sep)
  (let ([sep1 (add1 sep)])
    (string-join
     (for/list ([i rep])
       (string-append
        (string-join
         (for/list ([count (range (/ n sep1))])
           (string-append (make-string sep #\a) " ")))
        "\n")))))

(module+ sigh
  (define lol-1 (dotest (one-long 1000 1)))
  (define lol-10 (dotest (one-long 1000 10)))
  (define lol-100 (dotest (one-long 1000 10))))

; here be the quadratic things
(define (run-quad-test)
  "reveal that there is quadratic behavior lurking somewhere in brag probably in parser-tools"
  ; it is also possible that there is something in the org grammar specification that
  ; is triggering quadratic behavior but not entirely sure what, the fact that simply
  ; increaseing the number of tokens or decreasing the continuous read length is worrying 
  ; especially per line, maybe the + operator is what is causing the issue? hard to know/tell
  (for/list ([sep '(1000 500 400 300 200 100 50 40 20 10 9 8 7 6 5)])
    (let-values ([(out cpu real gc) (time-apply dotest-q (list (with-sep 1000 1 sep)))])
      (list sep cpu real gc))))

(module+ quadratic
  #;
  (define oof (run-quad-test))
  #;
  (set! oof (run-quad-test))

  ; and here we see the superiority of let
  ; for still actually being able to do lisp things
  (let ([oof (run-quad-test)])
    (values oof
            (plot (list (axes)
                        (points (map (λ (l) (take l 2)) (drop oof 5)))))))

  )

#; ; lol don't run this
(define sep-1 (dotest (with-sep 1000 1 1)))
