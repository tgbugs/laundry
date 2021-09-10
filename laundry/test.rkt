#lang racket/base

(require racket/list
         racket/file
         racket/path
         racket/pretty
         racket/string
         ;errortrace ; FIXME this is useless for debugging the parser
         ; also VERY slow, profile was spending 95% of its time there
         ; the only other thing that even comes close is something from stxparam
         ; ah, the slowness in racket-mode is because error-trace is enabled by default
         ; FIXME consider (compile-context-preservation-enabled) ??
         laundry/parser
         (only-in laundry/tokenizer
                  laundry-tokenizer-debug
                  laundry-make-tokenizer
                  laundry-final-port
                  bind-runtime-todo-keywords)
         #;
         (except-in laundry/expander #%module-begin)
         (rename-in (only-in laundry/heading make-rule-parser)
                    [make-rule-parser heading-rule-parser])
         syntax/strip-context
         (for-syntax
          racket/base
          (only-in racket/port with-output-to-string)
          syntax/parse))

(provide dotest dotest-q dotest-file dotest-quiet)

(define testing-parse (make-parameter parse))
(define testing-token (make-parameter laundry-make-tokenizer))
(define dotest-prefix (make-parameter #f))
(define dotest-suffix (make-parameter #f))
(define dotest-quiet (make-parameter #t)) ; parameters across require ... ?

(define-syntax (current-module-path stx)
  (syntax-parse stx
    [(_)
     #`(displayln
        #,(datum->syntax
           stx
           (with-output-to-string
             (λ ()
               (display "Running tests in module: ")
               (display (variable-reference->module-path-index (#%variable-reference)))))))]))

#;
(define testing-parse-to-datum (make-parameter parse-to-datum))
(define (rec-cont tree atom)
  ; I'm lazy and not going to write a proper bfs
  (string-contains? (pretty-format tree) (symbol->string atom)))


(define-namespace-anchor anc-test)
(define ns-test (namespace-anchor->namespace anc-test))

(define (dotest test-value
                #:eq  [eq #f]  #:eq-root            [eq-root #f]
                #:nt  [nt  #f] #:node-type          [node-type          nt]
                #:nte [nte #f] #:node-type-expanded [node-type-expanded nte]
                #:quiet [quiet #f]
                #:parse-to-datum [parse-to-datum #f]
                #:port-count-lines? [port-count-lines #t]
                ; FIXME parameterize do-expand (define test-expand (make-parameter #t))
                #:expand? [do-expand #t])
  (define test-value-inner
    (let ([p (dotest-prefix)]
          [s (dotest-suffix)])
      (let ([prepended (if p (string-append p test-value) test-value)])
        (if s (string-append prepended s) prepended))))
  (define (t) ((testing-token)
               (let ([port (open-input-string test-value-inner)])
                 (when port-count-lines
                   (port-count-lines! port))
                 port)))
  (define hrm ((if parse-to-datum ; used for debug
                   parse-to-datum
                   (compose syntax->datum (testing-parse)))
               (t)))
  (when (and node-type (not (rec-cont hrm node-type)))
    (error (format "parse of ~s does not contain ~s" test-value node-type)))
  (when (and eq (not (equal? eq hrm)))
    (error (format "foo ~s ~s" test-value-inner hrm)))
  ; WAIT!? setting a parameter modifies the parent in module+ but not
  ; if you actually require the module !? ARGH!
  (unless (or quiet (dotest-quiet))
    (pretty-write (list 'dotest: hrm)))
  (if (or do-expand node-type-expanded)
      (when (or do-expand node-type-expanded) ; LOL when x vs begin ...
        (define modname (string->symbol (format "org-module-~a" (gensym))))
        (define hrms
          (strip-context ; required to avoid hygene errors in expand and eval-syntax below
           ; for some reason raco make doesn't seem to care
           #`(module #,modname laundry/expander
               #,(parameterize ([laundry-final-port #f])
                   ((testing-parse) ; watch out for the 2 arity case in the case-lambda here
                    #; ; NAH just a completely insane case-lambda
                    ; that causes the call to revert to the full grammar
                    (format "test-source ~s" test-value-inner)
                    (t))))))
        (parameterize ([current-namespace ns-test])
          (unless (or quiet (dotest-quiet)) ; FIXME this logic is broken
            (pretty-write (list 'expanded:
                                (syntax->datum (expand hrms)))))
          #; ; FIXME for whatever reason drracket cannot manage to use eval-syntax here
          ; so we use eval syntax->datum instead, sigh
          (eval-syntax hrms)
          (eval (syntax->datum hrms))
          (define root (dynamic-require (list 'quote modname) 'root))
          (when (and node-type-expanded (not (rec-cont root node-type-expanded)))
            (error (format "expansion of ~s does not contain ~s" test-value node-type-expanded)))
          (when (and eq-root (not (equal? eq-root root)))
            (error (format "foo ~s ~s" test-value-inner root)))
          (unless (or eq-root node-type node-type-expanded)
            root)))
      (unless (or eq node-type)
        hrm)))

#;
(require debug/repl)
(define (dotest-q test-value)
  (dotest test-value #:quiet #t))

(define (dotest-fail test-value)
  (unless (with-handlers ([exn? (λ (exn) #t)])
            (dotest test-value)
            #f)
    (error "Should have failed.")))

(define (dotest-file path #:eq [eq #f] #:parse-to-datum [parse-to-datum #f])
  ; FIXME super inefficient
  #;
  (define (t) (laundry-make-tokenizer (open-input-string (file->string (string->path path) #:mode 'text))))
  (define hrm
    (with-input-from-file (expand-user-path (string->path path))
      (λ ()
        (port-count-lines! (current-input-port)) ; XXXXXXXXXXXXXX YAY this is what causes our backtracking issues :D :D
        (parameterize ([laundry-final-port #f])
          (let ([t (laundry-make-tokenizer (current-input-port))])
            ((if parse-to-datum
                 parse-to-datum
                 (compose syntax->datum (testing-parse)))
             path
             t))))
      #:mode 'text))
  (if eq
      (unless (equal? eq hrm)
        (error (format "path bar ~s" path)))
      #;
      (pretty-print hrm)
      hrm))

(module+ test-bof
  (current-module-path)
  ; fooing annoying as foo having to duplicate the whole fooing grammar
  ; just for this one fooing little special case FOO
  (dotest "")
  (dotest " ")
  (dotest " \n")
  (dotest "* ") ; XXX broken in the rework due to bof eof combo issues
  (dotest "* Headline") ; XXX broken in the rework due to bof eof combo issues
  (dotest "* Headline\n:properties:\n:poop: foo\n:end:") ; LOL broken again after it was fixed woo ambiguity
  (dotest "Paragraph foo poop.")
  (dotest "1. ordered list")
  (dotest "- descriptive list")
  (dotest "| table ")

  ; bof eof same line problem
  (dotest ":drawer:\n:end:") ; so this one works with bof-eof
  (dotest "\n:drawer:\n:end:") ; FIXME but now this one is wrong?
  (dotest ":drawer:\n:end:\n")

  )

(module+ test-wat
  (current-module-path)
  (dotest "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")

  (dotest "x. ")
  (dotest "x   ")
  (dotest "abc1")
  (dotest "abcd")
  (dotest "a.. ")
  (dotest "a.  ") ; XXX
  (dotest ":d:\n* \n:end:")
  (dotest "\n:d:\n* \n:end:")
  (dotest " \t ")
  (dotest "   ")
  (dotest "    ") ; XXX
  (dotest "     ") ; XXX
  (dotest "      ") ; XXX
  (dotest "       ") ; XXX
  (dotest "        ") ; XXX
  (dotest "x.  ") ; XXX
  (dotest "x.  \n")
  )

(module+ test-list
  (current-module-path)
  (define node-type 'plain-list-line)
  (dotest "0." #:node-type node-type)
  (dotest "0)" #:node-type node-type)
  (dotest "0. " #:node-type node-type)
  (dotest "0.  " #:node-type node-type) ; XXX neg srcloc
  (dotest " * poop" #:node-type node-type)
  (dotest " - foo" #:node-type node-type)

  (dotest "A. " #:node-type node-type)
  (dotest "A.  " #:node-type node-type) ; XXX neg srcloc
  (dotest "A. asdf" #:node-type node-type)
  (dotest "a. " #:node-type node-type)

  (dotest "1. \n 2. " #:node-type node-type) ; XXX neg srcloc

  (dotest " A. asdf" #:node-type node-type)
  (dotest " a. " #:node-type node-type)
  (dotest "1. \n a. " #:node-type node-type) ; XXX neg srcloc
  (dotest "1. There\n a. asdf" #:node-type node-type) ; XXX
  (dotest "1. There\n A. asdf" #:node-type node-type) ; XXX
  (dotest "1. There\n A. asdf\n" #:node-type node-type)

  )

(module+ test-npnn
  (current-module-path)
  (dotest "asdf" #:node-type 'paragraph)
  (dotest ";alksdjf;l jd; j;1oj;oij10j [p0j asd;foja ;kjas.d/f a.ldfjaoiejf01923jOAJ--1!@@#$%^&*[]{}\\/" #:node-type 'paragraph)
  (dotest "\n")
  (dotest "\t")
  (dotest " ")
  (dotest "|" #:node-type 'table)
  )

(module+ test-cell
  (current-module-path)
  ; from this it seems that we can't do PIPE? at the end because it will be eaten
  ; nope, not true, the PIPE? takes precednece so that does work in the limited case
  (dotest "|" #:eq-root '(org-file (table (table-row (table-cell)))))
  (dotest "||" #:eq-root '(org-file (table (table-row (table-cell)))))
  (dotest "|||" #:eq-root '(org-file (table (table-row (table-cell) (table-cell)))))
  (dotest "||||")
  (dotest "|||||")
  (dotest "|||||||||||||||||||||||||||||||||||||")
  (dotest "||||||||||||||||||||||||||||||||||||||")

  (dotest "|a")
  (dotest "|a|")
  (dotest "|a||")
  (dotest "||a|")
  (dotest "||a|b")
  (dotest "||a|b|")

  (dotest "|||oops|||oops|||oops oops|||")
  (dotest "|a\n")
  (dotest "| hello there")

  (dotest "|a ")
  (dotest "\n|a")

  (dotest "| a | yo")

  (dotest "| a | yo |")

  (dotest "| \\| |")

  (dotest "| - |")

  (dotest "| oh it is *bad /yes/ _it_ =is=* <https://> [[(oh-boy)]] |")

  )

(module+ test-row
  (current-module-path)
  (dotest "|\n|" #:eq-root '(org-file (table (table-row (table-cell)) (table-row (table-cell)))))
  (dotest "||\n|")
  (dotest "||\n||")
  (dotest "|a|\n||")
  (dotest "|a|\n|c|")
  (dotest "|a|b\n|c|")
  (dotest "|a|b\n|c|d")
  (dotest "|a|b|\n|c|d|")

  )

(module+ test-span-misalignment
  ; FIXME I think I'm triggering a bug in brag
  ; but it is happened because somehow the newline at eof is not triggering
  ; not a bug in brag, it was a -1 file position

  ; so I thik that this is happening because start-pos and end-pos are
  ; not in order resulting in a negative span, this is in brag-lib/codegen.rkt
  ; whole-rule-loc
  ; positions->srcloc
  ; rule-components->syntax

  (dotest   "|\nq") ; oooh that's bad
  (dotest   "|\nqq") ; oooh that's bad
  (dotest   "|\nq\n")
  (dotest   "|\nqq\n")

  (dotest "xx")
  (dotest "\n|\nyy\n")
  (dotest   "|\nzz\n")
  (dotest "\n|\npp")
  )

(module+ test-table
  (current-module-path)

  (dotest "|\nx ")
  (dotest "|\n\nx")

  (dotest "|\nxx") ; oooh that's bad XXX 99% that this is a newline at eof related issue
  (dotest "|\nxx\n")
  (dotest "|\nx x")

  ; test to make sure that the stop before clause does zero or more not newlines
  (dotest "   |\nx")
  (dotest "|\nx")

  (dotest " |\nx\n")
  (dotest "|\nx\n")
  (dotest "\n |\n x\n")
  (dotest "\n |\n !\n")

  (dotest "\n |\n x| \n")
  (dotest "\n |\n x|")
  (dotest "\nwat\n|\nx")
  (dotest "\n|\nx")
  (dotest "|\n")
  (dotest "|")
  (dotest "| ")
  (dotest " |")
  (dotest " | ")
  (dotest "|t\n|2\n|-\n|3")
  (dotest "|i|am|a|table")
  (dotest "|i |am|a|table ")
  (dotest "| t")
  (dotest "|  t")
  (dotest "|  t|")
  (dotest "|t|")
  (dotest "|t|\n")
  (dotest "|t ")
  (dotest "|table ")
  (dotest "|table")
  (dotest "|ta")
  (dotest "|t")
  (dotest "|t\n")
  (dotest "|t\n|2")
  (dotest "|t\n|2|")
  (dotest "|\n")
  (dotest "|\n|")
  (dotest "|\n|\n")
  (dotest "|\n|\n||")
  (dotest "|\n|\n|||")
  (dotest "|\n|\n|||\n|")
  (dotest "||")
  (dotest "||\n||")
  (dotest "\n| the foo")
  ; FIXME the bug here is that these should all be one table but show
  ; up as multiple tables possibly due to ambiguity in the grammar
  (dotest "|ah poop| wut\n|is going| on here| oh hai mark") ;x
  (dotest "|ah-poop|-wut\n|is-going|-on-here|-oh-hai-mark") ;x
  (dotest "* \n|a|b  | c | d \n|e f g | hi|j|k\n|l\n|m|n\n|o\n|p") ;x
  (dotest "* \n|a|b  | c | d \n|e f g | hi|j|k\n|l\n|m|n\n|o\n|p") ;x
  (dotest "* \n|a|b  | c | d \n|e f g | hi|j|k\n|l\n|m|n\n|o\n") ;x
  (dotest "* \n|a|b  | c | d \n|e f g | hi|j|k\n|l\n|m|n\n") ;x
  (dotest "* \n|a|b  | c | d \n|e f g | hi|j|k\n|l") ;x
  (dotest "* 
|a|b  | c | d 
|e f g | hi|j|k") ; broken

  (dotest "||\n|||") ;ok
  (dotest "||\n|c|d|e") ;ok
  (dotest "||b\n|c|d| e") ;ok
  (dotest "|a|b\n|c|d| e") ;bad WAT XXX seriously wat

  (dotest "* \n|a|b  | c | d ")
  (dotest "* \n|a|b  | c | d |")
  (dotest "* \n|a|b  | c")
  (dotest "* \n|a|b")
  (dotest "* \n|a")
  (dotest "* \n|a\n")
  (dotest "* \n|a\n|b")
  (dotest "* \n|a\n|b c")
  (dotest "|tables\n|-\n|rule!")

  (dotest "|a\n|b\n|c\n\n|d\n|e") ; FIXME this should be two nodes
  (dotest "|\n\n|")
  (dotest "|\n\n\n|")


  (dotest "|a\n\nb|c")
  (dotest "|a\nb|c") ; tests stop-before correction
  (dotest "|a \n b|c")

  ; FIXME can't repro from test.org
  (dotest "
| can you have newlines in cells?
  this seems strange to me? | no, you can't
")

  ; test eof and table-element lex abbrev issues
  (dotest "|\n\n|\nx|")
  (dotest "\n|\n\n|\nx|")

  (dotest "\n|\n\n|\n x|\n")
  (dotest "\n|\n\n|\n x|")

  (dotest "|\n x|")

  (dotest "|\n x|\n\nwat")

  (dotest "\n|x|")
  (dotest "\n\n|x|")
  (dotest "\n:end:\n* \n|\nx|") ; can't repro
  (dotest "\n:end:\n* \n|\n x|") ; can't repro
  (dotest "\n:end:\n** \n|\n x|") ; can't repro

  (dotest "#lang org\n:end:\n*** \n|\nx|") ; can't repro
  (dotest "* \n:end:\n* \n|\nx|") ; can't repro

  (dotest ":a:\n* \n:b:\n* ") ; XXX this repros now

  (dotest "\n:a:\n* \n:b:\n* ") ; cant' seem to repro

  (dotest "
:a:
* b
:c:
* d
:e:
* f
")

  (dotest "#lang org
:a:
* b
:c:
* d
:e:
* f
")

  )

(module+ test-headline-content
  (current-module-path)

  (parameterize ([testing-parse
                  ; XXX watch out for the 2 arg part of case-lambda produced by make-rule-parser
                  (heading-rule-parser --test--heading-rest)]
                 [testing-token (bind-runtime-todo-keywords '("TODO" "DONE" "FUTURE"))]
                 [laundry-tokenizer-debug #f]
                 [dotest-prefix " "]
                 [dotest-suffix "\n"])
    (dotest "")
    (dotest ":")
    (dotest ": ")
    (dotest ":t:")
    (dotest ":n: :t:") ;
    (dotest "TODO")
    (dotest "DONE")
    (dotest "FUTURE")

    (dotest "DONE :t:")
    (dotest "DONE:t:") ;
    (dotest "DONE :n: :t:") ;

    (dotest "DONE T")
    (dotest "DONE COMMENT T")
    (dotest "DONE [#P] COMMENT T")
    (dotest "DONE [#P] COMMENT T :n: :t:") ;
    (dotest "DONE [#P] COMMENT Title")
    (dotest "DONE [#P] COMMENT Title :n: :t:") ;

    (dotest "DONE[#P] COMMENT Title")
    (dotest "DONE[#P] COMMENT Title :tag:")

    (dotest "DONE[#P] COMMENT T :t:")
    (dotest "DONE[#P] COMMENT T :t:")

    (dotest "DONE Title")
    (dotest "DONE Title :t:")

    (dotest "T :n: :t:")
    (dotest "T :t:")
    (dotest "T n:t:")

  ))

(module+ test-planning ; FIXME very broken now
  (current-module-path)
  (dotest "* H\nDEADLINE:")
  (dotest "* H\nSCHEDULED:")
  (dotest "* H\nCLOSED:")
  (dotest "* H\nDEADLINE:SCHEDULED:CLOSED:")
  (dotest "* H\nDEADLINE: SCHEDULED: CLOSED:")

  (dotest "* H\nDEADLINE: <2020-08-11> SCHEDULED: <0100-03-01> CLOSED: <1873-09-12>")

  (dotest "* H\nOPENED:")
  (dotest "* H\nOPENED: DEADLINE: SCHEDULED: CLOSED:")
  (dotest "* H\nOPENED: <2012-09-16> DEADLINE: <2020-08-11> SCHEDULED: <0100-03-01> CLOSED: <1873-09-12>")

  (dotest "* H\nDEADLINE: <2020>") ;x FIXME shouldn't parse fail should -> malformed line but I think we can't quite do that
  (dotest "* H\nDEADLINE: <2020-11>") ;x
  (dotest "* H\nDEADLINE: <2020-11-1>") ;x
  (dotest "* H\nDEADLINE: <2020-11-01>")
  (dotest "* H\nDEADLINE: <2020-11-01 ASDF>")

  )

(module+ test-priority
  (dotest "* [#A]")
  (dotest "* [#A]Title")
  (dotest "* [#A]:not_a_tag:")
  (dotest "* [#A] :a_tag:") ; FIXME why no hyphen? opporunity for regularization or what is going on?
  (dotest "* TODO [#A]")
  (dotest "* x [#A]")
  )

(module+ test-headline
  (current-module-path)
  ; FIXME not sure what COMMENT breaks things ?!
  ;(define test-value "this is ORG MODE\n* headline\n YEAH\n** COMMENT comment headline\nstuff\n* hl2 :tag:\n")
  ; ok, so BOF and EOF are causing annoying edge cases
  #;
  (dotest-quiet #f)

  (dotest "** COMMENT[#A]")
  (dotest "** COMMENT [#A] COMMENT")

  (dotest "*                      :x:")

  (dotest "* " #:node-type 'headline)
  (dotest (string-append (make-string 99 #\*) " "))
  (dotest "* H")
  (dotest "* H ") ; how the foo does this work
  (dotest "* He") ; and this work
  (dotest "* He ") ; but this fail?
  (dotest "* T n:t:")

  (dotest "* [#P]:t:")
  (dotest "* [#P] :t:")

  (dotest "* H [#P]:t:") ; x
  (dotest "* H [#P] :t:") ; x

  ; not the immediate issue
  (dotest "* [#P]:ta:")
  (dotest "* [#P]:t:1:")

  (dotest "* [#P]COMMENTT") ; ok
  (dotest "* [#P]COMMENTTi") ; ok

  (dotest "* [#P]COMMENTTi :t:") ; broken
  (dotest "* [#P]COMMENTT :t:") ; broken
  (dotest "* [#P]COMMENTT :t: ") ; broken
  (dotest "* [#P]COMMENTT  :wat:asdf: ") ; broken
  (dotest "* [#P]COMMENTT :ARCHIVE:") ; ok !?

  (dotest "* Headline [#P]COMMENT Title")

  (dotest "* H [#P]COMMENT T")
  (dotest "* H [#P]COMMENT Ti")
  (dotest "* H [#P]COMMENT Ti :t:")
  (dotest "* H [#P]COMMENT T :t:")
  (dotest "* H [#P]COMMENT :t:")
  (dotest "* H [#P]COMMENT:t:")
  (dotest "* H [#P]COMMENT:ta:")
  (dotest "* H [#P]COMMENT:t: ")
  (dotest "* H [#P]COMMENT:ARCHIVE:")

  ; the behavior of org-export is clear here, no spaces -> title
  (dotest "* TODO[#P]COMMENT T")
  (dotest "* TODO[#P]COMMENT Ti")
  (dotest "* TODO[#P]COMMENT Ti :t:")
  (dotest "* TODO[#P]COMMENT T :t:")
  (dotest "* TODO[#P]COMMENT :t:")
  (dotest "* TODO[#P]COMMENT:t:")
  (dotest "* TODO[#P]COMMENT:ta:")
  (dotest "* TODO[#P]COMMENT:t: ")
  (dotest "* TODO[#P]COMMENT:ARCHIVE:")

  (dotest "* COMMENTARY") ; the poor sods

  (dotest "* TODO:tag:")

  ; wow wtf these are max spook
  (dotest "* H [#P]COMMENTT")
  (dotest "* H [#P]COMMENTTi")
  (dotest "* H [#P]COMMENTT :t:")

  (dotest "*   [#P]COMMENT Sigh")
  (dotest "* H [#P]COMMENT Sigh :tag:")
  (dotest "*   [#P] COMMENT Sigh")
  (dotest "*   [#P]  COMMENT Sigh")
  (dotest "* H [#P]  COMMENT Sigh :tag:")

  (dotest "* H [#P]\n") ; broken again
  (dotest "* H [#P] \n") ; broken again
  ; still amgibuous
  (dotest "* H [#P] T\n")
  (dotest "* H [#P] T :tag:\n")
  (dotest "* H [#P]   :tag:\n")

  ; FIXME this should parse with the todo keyword ???? XXX fixed now, the clauses were out of order (duh) ; XXX FIXME broke it again
  ; because we don't know the actual TODO keyword values at runtime AND because title will gobble
  ; the priority ... this is tricky ... yes ... yes it is

  ; ok
  (dotest "* [#B] COMMENT YEAHH!!!!\n")
  (dotest "* [#H] COMMENT a a1 aaa :tag: \n") ; (ref:break-1)
  (dotest "* [#M] COMMENT wat :tag: \n")
  (dotest "* [#O] COMMENT wat  :tag: \n")
  (dotest "* [#N] COMMENT wat 1 :tag: \n") ; WAT a todo keyword !? also LOL this can actually happen
  (dotest "* [#P] COMMENT wat   :tag: \n") ; WAT a todo keyword ?! also LOL this can actually happen
  ;; annoyingly inconsistent
  (dotest "* [#G] COMMENT :tag: \n")
  ; behavior doesn't matches elisp but that behavior is not consistent with null title
  ; I have moved h-comment to its own element not nested inside the title and then opened
  ; up the possibility to have tags that are ending
  (dotest "* [#C] COMMENT :some:tag:\n") ; wait what the foo?



  ; ambig
  (dotest "* [#A] Urg\n")
  (dotest "* [#D] Urg :tag:tag:tag:\n")
  (dotest "* [#I] aaaaaaaaa a1aaaaaa aaaaaaa :tag: \n")
  (dotest "* [#J] wat :tag: \n")
  (dotest "* [#K] wat wat :tag: \n")
  ; so there is some seriously broken poop going on here? there is NO way that
  ; adding an additional wat should break any of this ...
  (dotest "* [#L] wat wat wat :tag: \n")
  (dotest "* [#K] wat wat wat :tag: \n")

  ; always ambig since we don't use title unprefixed
  (dotest "** Headline level 2\n" #:nte 'heading)

  (dotest "why can't we share this newline?\n** Headline level 2\n" #:nte 'heading)

  ;; broken
  (dotest "* Headline Sigh\n") ; this should hit todo-keyword but is gobbled just the title

  (dotest "* [#F] so apparently this tag is valid :tag: \n") ; how the foo is this broekn again

  (dotest "* [#E] NOOOO :1:2:3: derp\n") ; broken !?
  (dotest "* Headline [#Z] Sigh :T:A:G:S:\n") ; FIXME SOMEHOW adding tags makes the todo-keyword work !??!?!
  (dotest "* Headline [#Z] :T:A:G:S:\n")
  (dotest "* Headline [#Z]Sigh") ; broken
  (dotest "* Headline [#Z]Sigh :t:\n") ; now broken

  (dotest "* : H")
  (dotest "* : H ")
  (dotest "* [ H")
  (dotest "* [ H ")
  (dotest "* ] H")
  (dotest "* ] H ")

  (dotest "* :")
  (dotest "* : ")
  (dotest "* [")
  (dotest "* [ ")
  (dotest "* ]")
  (dotest "* ] ")

  (dotest "* 
** 
*** 
**** 
****** 
******* 
******** 
********* ")

  ; watch out for negated patterns that don't include a newline in the negated set >_<
  (dotest "a\nb\nc\nd\n* \ne\nf\ng\n****** h\ni" #:nte 'heading)
  (dotest "* \ne\nf\ng\n****** h\ni" #:nte 'heading)
  (dotest "g\n****** h" #:nte 'heading)
  (dotest "g\n***** h" #:nte 'heading)
  (dotest "\n****** h\ni" #:nte 'heading) ; this one ok in most cases anyway

  )

(module+ test-paragraph-start
  (current-module-path)
  (laundry-tokenizer-debug #f)

  (dotest "  #+begin_src")
  (dotest "  #+begin_srclol" #:node-type 'paragraph)
  (dotest "  #+begin_src\n")
  (dotest "  #+begin_")
  (dotest "  #+begin_-") ; -> block
  (dotest "  #+begin_:") ; -> block FIXME keyword line confict XXX ambig

  (dotest "#+end_ " #:node-type 'malformed) ; ok
  (dotest "  #+begin_src oops not a thing") ; ok
  (dotest "  #+begin_ more" #:node-type 'paragraph) ; ok

  ; FIXME dynamic block weirdness
  (dotest "#+end:" #:node-type 'keyword) ; -> keyword
  (dotest "  #+end:" #:node-type 'keyword)

  (dotest "  #+end") ; FIXME dynamic block end, possibly a bug in the dynamic bock spec
  (dotest "#+end")
  (dotest "  #+end_")
  (dotest "#+end_")
  (dotest "#+end_srclol" #:nte 'paragraph)

  (dotest "#+:end::properties::end: lol" #:node-type 'keyword) ; -> keyword
  (dotest "#+:end:" #:node-type 'keyword) ; -> keyword
  (dotest "#+:properties:" #:node-type 'keyword) ; -> keyword
  (dotest "#+:end::asdf")
  #; ; in our current implementation this is a keyword-line what the key and value actually parse to ???
  (dotest "#+:end::asdf" #:node-type 'paragraph) ; XXX inconsistnet org element says keyword, font locking and spec say paragraph


  (dotest (make-string 99 #\*)) ; FIXME ideally nested markup should issue a warning and not nest in the tree
  ; now fixed and blazingly fast but
  ; but wow NAME+ has horrible performance, probably need to move more of this stuff to the lexer
  ; LOL at 99 it is bad, at 999 THIS IS A DISASTER for performnace and memory usage
  (dotest " *********")

  (dotest " ********* ")
  (dotest " ** ")
  (dotest "*********")
  (dotest " *\n")
  (dotest " *")
  (dotest " * ")

  (dotest "  #" #:node-type 'comment) ; -> comment XXX FIXME broken comment element should be matching this
  (dotest "#" #:node-type 'comment) ; -> comment
  (dotest "# " #:node-type 'comment) ; -> comment
  (dotest "  #a" #:node-type 'paragraph)
  (dotest "#a" #:node-type 'paragraph)
  (dotest "  #+")
  (dotest "#+")
  (dotest "#+:")
  (dotest "#+:aaaa:")
  #; ; correct but not fully implemented yet
  (dotest "#+:aaaa:" #:eq-root '(org-file (keyword (keyword-key ":aaaa"))))
  (dotest "#+:aaaa")
  (dotest "#+::")
  (dotest "#+: :")
  (dotest "#+ :")
  (dotest "#+a :")
  (dotest "#+a : asdf :")
  (dotest "#+a hello there: YEAH")

  ; ugh big tokes and drawer tokens
  (dotest "  #+call:")
  (dotest "  #+call:eeeeeeeeee")
  (dotest "  #+calla") ; XXX TODO malformed case
  (dotest "#+:end")
  (dotest "#+:end: lol: oops") ; -> keyword
  (dotest "#+:end: asdf") ; -> keyword
  (dotest "#+:end:lol: oops") ; -> keyword
  (dotest "#+:properties: lol: oops") ; -> keyword
  (dotest "#+:properties:lol: oops") ; -> keyword

  (dotest "  -") ; -> descriptive-list
  (dotest "-") ; -> descriptive-list
  (dotest "  -a")
  (dotest "-a")
  (dotest "  +") ; -> descriptive-list
  (dotest "+") ; -> descriptive-list
  (dotest "  +a")
  (dotest "+a")
  (dotest "  *") ; -> descriptive-list
  (dotest "*" #:node-type 'paragraph) ; interestingly this parses as a paragraph line in elisp too, it can't ever be a plain list
  (dotest "  *a" #:node-type 'paragraph)
  (dotest "*a" #:node-type 'paragraph)
  (dotest "* ") ; -> headline

  (dotest "  |" #:node-type 'table)
  (dotest "|" #:node-type 'table)
  (dotest "|lol" #:node-type 'table)

  (dotest "0")
  (dotest "  9")
  (dotest "  9.") ; -> ordered list
  (dotest "9.") ; -> ordered list
  (dotest "  9)") ; -> ordered list
  (dotest "9)") ; -> ordered list
  (dotest "  9.a")
  (dotest "9.a")
  (dotest "  9)a")
  (dotest "9)a")
  (dotest "9)#+end:")
  (dotest "9)ARCHIVE")
  (dotest "  9999999." #:nte 'ordered-list-line) ; -> ordered list XXX FIXME broken ??!
  (dotest "9.") ; -> ordered list
  (dotest "99")
  (dotest "99._")
  (dotest "99 _")
  (dotest "999999 hello world")
  (dotest "99. ") ; -> ordered list
  (dotest "99." #:nte 'ordered-list-line) ; -> ordered list
  (dotest "999.") ; -> ordered list
  (dotest "9999.") ; -> ordered list
  (dotest "99999.") ; -> ordered list
  (dotest "999999.") ; -> ordered list
  (dotest "9999999." #:nte 'ordered-list-line) ; -> ordered list
  (dotest "  9999999.a")
  (dotest "9999999.a")

  (dotest "  COMMENT")
  (dotest "COMMENT")
  (dotest "COMMENT more")
  (dotest "  ARCHIVE")
  (dotest "ARCHIVE")
  (dotest "ARCHIVE more")
  (dotest "  :ARCHIVE")
  (dotest ":ARCHIVE")
  (dotest ":ARCHIVE more")

  (dotest " (a)" #:node-type 'paragraph)
  (dotest " (" #:node-type 'paragraph)
  (dotest " )" #:node-type 'paragraph)

  (dotest " _" #:node-type 'paragraph)

  (dotest ".\n," #:node-type 'paragraph)
  (dotest ".\n ," #:node-type 'paragraph)
  (dotest "," #:node-type 'paragraph)
  (dotest "." #:node-type 'paragraph)

  (dotest ".a" #:node-type 'paragraph)
  (dotest ".ab" #:node-type 'paragraph)
  (dotest " .a" #:node-type 'paragraph)
  (dotest " .ab" #:node-type 'paragraph)

  )

(module+ test-paragraphs
  (current-module-path)
  (define nte 'paragraph)
  (dotest "aaaaaaaaa paragraph" #:nte nte)

  (dotest "p11\np12\n\np21"
          #:eq-root
          '(org-file (paragraph "p11" "\n" "p12" "\n") (paragraph "\n" "p21")))

  (dotest "\nIf nothing else this is a paragraph." #:nte nte)
  (dotest "p" #:nte nte) ; sigh bof
  (dotest "\np")
  (dotest "\np\n")
  (dotest "\np a r a g p\n")

  (dotest "p a r a g p")
  (dotest "aaaaaaaaaaa")
  (dotest "aaaaaaaaaaaa")
  (dotest "0123456789")
  (dotest "0123456789A")
  (dotest "01234567890")
  (dotest "0123456789")
  (dotest "0123456789A")
  (dotest "I'm sorry I really just do not understand what the issue is here.")

  (dotest "
* I don't get it
Seriously what the foo?
")

  (dotest "\n#+NAME: hello there\nGeneral Kenobi")

  (dotest "\n#+ATTR_HTML: hello there\nGeneral Kenobi")

  (dotest "
#+ATTR_FOO: POO
AAAAAAAAAAAAAAAAAAAAAAA

* Why is this so fooed?
   asdf")

  ; the parser reads everything twice the only difference in the token stream
  ; is whether or not the newline is there what the foo?
  (dotest "\n\np\n\n")
  (dotest "\n0123456789A")
  (dotest "0123456789A")
  (dotest "paragraph\n")

  )

(module+ test-markup
  (current-module-path)

  (dotest "*")
  (dotest "**")
  (dotest "***")

  (dotest " *hello* ")
  (dotest " /hello/ ")
  (dotest " _hello_ ")
  (dotest " +hello+ ")
  (dotest " =hello= ")
  (dotest " ~hello~ ")

  (dotest " ~hello~ there")

  (dotest "a *b* /c/ _d_ +e+ =f= ~g~ ")

  (dotest " ~hello~ there")
  (dotest "/hahaha/")
  (dotest " /asdf/")
  (dotest "\n/asdf/")
  (dotest "\n**oops\n* hello\nOH NO *lol*")

  (dotest "*/bi/*")
  (dotest "/*ib*/")
  (dotest "/*_ibu_*/")
  (dotest "_*/ubi/*_")

  (dotest "*/_+bius+_/*")

  (dotest "*bold text /bi text/ bold _bu text_ bold =bold verb= bold ~bold code~ bold*")

  (dotest "*b /i _u +s =v /*_+lol+_*/= ~c /*_+lol+_*/~ s+ u_ i/ b*")

  (dotest "\n =v=")
  (dotest "\n =v=\n")
  (dotest "*b*\n =v=")
  (dotest "*b*\n =/v/=")

  (dotest " *b*")
  (dotest " =v=")
  (dotest "   =v=")

  (dotest "*b*")
  (dotest "/i/")
  (dotest "_u_")
  (dotest "+s+")
  (dotest "=v=")
  (dotest "~c~")

  (dotest "d/f \\/")
  (dotest "d*f \\*")

  (dotest "=x= =y=")
  (dotest "=x=  =y=")
  (dotest "=x= /z/ =y=" #:nte 'italic)

  (dotest "~x~ /z/ ~y~" #:nte 'italic)

  (dotest "*x* /z/ *y*" #:nte 'italic)
  (dotest "*x * /z/ *y*")

  (dotest "x * /z/ *y")

  (dotest "*")

  (dotest "_ * *")


  (dotest "*/_+b+_ _+bus+_ /*")

  (dotest "/_+x+_ _+bus+_ /")

  (dotest "x+_ _+bus+_")
  (dotest "x*_ _*bus*_")

  (dotest "*/_+bius+_ _+bius+_/*")

  (dotest "*/_+bius+_ _+bius+_ bi/*")

  )

(module+ test-macros

  (current-module-path)
  (dotest "{{{macro}}}")
  (dotest "{{{macro(a,b,c)}}}")

  )

(module+ test-comments
  (current-module-path)

  (dotest "hrm a paragraph
# then a comment
# another comment
then another paragraph
")

  (dotest "35934")
  (dotest "35934" #:eq-root '(org-file (paragraph "\n" "35934")))
  (dotest "# 35934")
  (dotest "# hello")
  (dotest "# hello\n# there\nwat")
  (dotest "\n#")
  (dotest "\n#\n\n")
  (dotest "\n# a\nwat")

  (dotest "\n# comment comment comment # lol")
  (dotest "
# comment 1
# comment 2
  #
 #
#")

  (dotest "\n# oops this is broken\n#+NAME: aaaaaaaaaaaaaaaaaaa\npoop")
  (dotest "\n# oops this is broken\n#+WAIT a gosh darned moment")
  (dotest "\n#+NAME: aaaaaaaa\n# hello there a named comment")

  (dotest "\n#not a comment" #:node-type 'paragraph)

  )

(module+ test-rando
  (current-module-path)
  (laundry-tokenizer-debug #f)

  (dotest "“
(k)
* h") ; very broken

  (dotest
   "* w
w1 [[* w][w]]
")

  (dotest
   "* w
[[* w][w]] w2
")

  (dotest
   "* w
[[* w][w]]
")

  ; port-count-lines! related issue
  ; the knights who say ni!
  (dotest "হিনী \nx =s=\n* \n" #:port-count-lines? #f)
  (dotest "হিনী \nx =s=\n* \n")
  (dotest "হিনী ")

  (dotest "* \n <x")
  (dotest "=emerge --onlydeps ${packagename}=")
  (dotest "* Headline")
  (dotest "1.")

  (dotest "This is a paragraph sigh.") ; lol this breaks because there is something else that expects yet another newline

  ;(dotest "")

  (dotest "1. a plain list maybe?")

  ;(dotest "")

  (dotest "0.
yes
1. a list of things
   maybe
2. other
   more
Less?
1. or is it 3?
")

  (dotest "
* Headline 1
:property:
confusingly
:end:
Paragraph
** Headline 2
") ; FIXME now drawers are broken for foos sake

  (dotest "
this is ORG MODE\n* headline\n YEAH\n** comment headline\nstuff\n* hl2 :tag:
# this is a comment line 1\n# line 2\n\n")

  (dotest "\n??$>]\n\thello world")
  (dotest "\n@@@@")
  (dotest "\nasdf @ hello there @ lol")
  (dotest "\n@@asdf: wat@@")
  (dotest "
@@comment: ok this is cool
yeah don't you agree? look @ that!
@@")
  (dotest "\neven cooler are you ready? @@latex: OH
HELL YES 
look at all these newlines
@@ and now for something completely different!")

  (dotest "
OH NO
#+begin_src elisp
 (+ 1 2)
#+end_src
WHEEEEEEEEEE
" #:nte 'blk-src) ; FIXME now broken

  (dotest "
BEF PAR
:drawer:
lol
:end:
AFT PAR
" #:nte 'drawer)

  (dotest "lolololol")

  (dotest "
trolololol
watch this ::


")

  (dotest "
tralalalaala the
watch this ::


")

  (dotest "\n#+NAME: lol\nare you fooing kidding me!")

  )

(module+ test-drawers
  (current-module-path)
  (laundry-tokenizer-debug #f)

  (dotest "\n:drawer:\n:end:")

  (dotest ":d:\n:end:" #:nte 'drawer)
  (dotest ":d:\n* \n:end:"
          #:nte 'heading) ; foo yeah it works ; FIXME malformed ?

  (dotest ":ARCHIVE:\n:end:" #:nte 'drawer)

  (dotest "
* Headline 1
:properties:
:olo: lol
:end:" #:node-type 'property-drawer)

  (dotest "
* asdf
:properties:
:hrm: oops :end:" #:nt 'paragraph)

  ; FIXME TODO perfect example of why we will also need a way to chain a newline when we peek the eof
  (dotest "
* Headline 1
:properties:
    :end:")

  (dotest
   ; FIXME why does :header-args:python+: asdf lower the affinity of the property-drawer relative to the plain drawer
   ; despite the fact that property-drawer clearly matches? Is the grammar compiler making a sly optimizaiton choice?
   ; can we force it to _not_ make that choice? well the parse is consistent with the grammar
   ; there is no clear way to say "drawer directly attached to headline with :properties: or :PROPERTIES: is always property-drawer"
   ; I'm BETTING that what happens is that it backtracks ALL THE WAY on the first failure instead of part way, so if there is _any_
   ; failure, then it rolls forward to see if it can find another correct parse
   ; XXX lol ok, it looks like there was something else that was fooed up in my grammar and it is fixed now
   ; so all this angst for nothing?
   "
******** Oops
:properties:
:oh: yeah
:header-args:python+: :var hrm=1  
:end:")

  (dotest "
******** Ok
:properties:
:oh: yeah
:end:")
  ; FIXME it seems I managed to break property drawers again
  (dotest "
******** Oops
:properties:
:oh:no: !? actually ok?! :wait for it
:end:")

  (dotest "
** Also ok!?
:properties:
:key: :var oops
:end:")

  (dotest "
** Also ok!?
:properties:
:key: :a b :c d
:end:")

  (dotest
   ; what the foo is this :/
   "
** Also ok!?
:properties:
:k1:k2+: x ! ?! :ijk b c
:end:")

  (dotest
   "
******** Oops
:properties:
:oh:noo: !? actually ok?!
:oh-noo: !? actually ok?!
:end:"
   )

  (dotest
   "
******** Oops
:properties:
:oh-noo: !? actually ok?!
:end:"
   )

  (dotest
   "
* wat
:properties:
:k: ab
:end:")

  (dotest
   "
* drawer test
:properties:
:: just a drawer
:end:"
   #:node-type 'drawer)

  (dotest ; woah this works with nested parsing
   ; org will parse this as a property drawer
   "
* property node name issue
:properties:
:+: elisp impl supports this spec does not
:end:")

  ; issues with value
  (dotest "\n* \n:properties:\n:k+:\n:end:") ; broken
  (dotest "\n* \n:properties:\n:k+: \n:end:") ;ok
  (dotest "\n* \n:properties:\n:k+:  \n:end:") ;ok
  (dotest "\n* \n:properties:\n:k+: v\n:end:") ;ok

  (dotest
   "
* wat
:properties:
:k: ab
:end:

:another:
:end:
random end
:end:
")

  (dotest "
:random-drawer:

you called?
@@hrm: oops @@ in drawer context esinips are ignored
   :end:

:end:
") ; ok

  (dotest "\n:drawer:\n\n:end:")

  )

(module+ test-string
  (current-module-path)
  ; longest match kills using the tokenizer for this
  (dotest #<<--
"1 2"
--
          )

  (dotest #<<--
"1 \" 2"
--
          )

  (dotest #<<--
"1 \" 2"
--
          )

  )

#; ; slows the build, and most are tested in test-block-switches now
(module+ test-switches
  (current-module-path)

  #; ; this sets for the parent module as well but not on require?
  (testing-parse (make-rule-parser --test--switches-sane))

  (parameterize ([testing-parse
                  ; XXX watch out for the 2 arg part of case-lambda produced by make-rule-parser
                  (make-rule-parser --test--switches-sane)])

    (dotest "-r -q")
    (dotest "+r +q")
    (dotest "-r -l")
    (dotest "-r -l \"lol\\\"HAH \"")
    (dotest "-r -l \"lol\\\"HAH \" -h +e")
    (dotest "-r -l \"(ref:%s)\" +e -e") ; FIXME this is severly broken

    ))

(module+ test-block-switches
  (current-module-path)

  (define strings
    '("#+begin_src elisp -r -l \"(ref:%s)\" +e -e :noweb yes :tangle (some-file \"oh great\")"
      "#+begin_src elisp -r -l \"(ref:%s)\" +e -e"
      "#+begin_src elisp -r -l \"(ref\\\":%s)\" +e -e"
      "#+begin_src elisp -r"
      "#+begin_src elisp -r "
      "#+begin_src elisp -r -e"
      "#+begin_src elisp -r +e"
      "#+begin_src elisp -r +e         \" aa a wat"
      "#+begin_src elisp -r +e \""

      "#+begin_src elisp -r +e \"broken line "
      "#+begin_src elisp -r +1 \"this is still weird but not completely broken\""

      "#+begin_src elisp -l \"foo\""
      "#+begin_src elisp -l \"foo\" -s "
      "#+begin_src elisp -s -l \"foo\""
      "#+begin_src elisp -s -l \"foo\" -s "

      "#+begin_src elisp -l \"foo\" aaaaaaaaaaaaaaaaaa"
      "#+begin_src elisp -l \"foo\" -s aaaaaaaaaaaaaaaaaa"
      "#+begin_src elisp -s -l \"foo\" aaaaaaaaaaaaaaaaaa"
      "#+begin_src elisp -s -l \"foo\" -s aaaaaaaaaaaaaaaaaa"

      "#+begin_src elisp -l \"foo\" -lol"
      "#+begin_src elisp -l \"foo\" +lol"

      "#+begin_src elisp -1"
      "#+begin_src elisp -s -w \"wat\" -11111111111 eeeeeeeeeeeeeee"
      "#+begin_src elisp -s -w \"wat\" -s"
      "#+begin_src elisp -s -w \"wat\" -sa"
      "#+begin_src elisp -s -w \"wat\" -s-" ; x
      "#+begin_src elisp -s -w \"wat\" -s+" ; x

      "#+begin_src elisp -l \"foo\" -1"
      "#+begin_src elisp -l \"foo\" +1"
      "#+begin_src elisp -l \"foo\" + "
      "#+begin_src elisp -l \"foo\" - "

      "#+begin_src elisp -l \"foo\" -e - "

      ; just because I'm evil and we can
      "#+begin_src elisp -l \"foo\" -l \"you\""
      "#+begin_src elisp -s -l \"foo\" -l \"you\""
      "#+begin_src elisp -s -l \"foo\" -s -l \"you\""
      "#+begin_src elisp -s -l \"foo\" -s -l \"you\" -s"))
  (for/list ([s strings])
    (dotest s))
  (for/list ([s strings])
    (dotest (string-append s "\n#+end_src")))
  )

(module+ test-blk-dyn
  (current-module-path)
  (dotest "#+begin:\n#+end:") ; FIXME keyword not parsing correctly really
  (dotest "#+begin: \n#+end:")
  (dotest "#+begin:
:d:
drawer contents
:end:
#+end:")

  (dotest
   ; yep this is broken
   "#+begin: b
:d:
#+end:
:end:
#+end:")

  )

(module+ test-blocks
  (current-module-path)

  (dotest "
#+begin_h
* h
#+end_h
")

  (dotest "
#+begin_h
#+end_h
")

  (dotest "
#+begin_h
#+end_h
#+begin_c
#+end_c
")

  (dotest "
#+begin_h
asdf
#+end_h

#+begin_h
hrm
#+end_h
")

  (dotest "
#+begin_example org
,#+begin_src bash -r -l :noweb yes
(+ 1 2)
,#+end_src
#+end_example
")

  (dotest "
#+begin_src
#+end_src
")

  (dotest "
#+begin_src 
#+end_src
") ; with space

  (dotest "
#+begin_src lang
#+end_src
#+end_src
")

  (dotest "
#+begin_src 
#+end_src
#+end_src
") ; note the space after #+begin_src it produces different behavior
  ; this is due to the difference 

  (dotest "
#+begin_src
#+end_src
#+end_src
")

  (dotest "
#+begin_src
in
#+end_src
out
#+end_src
")


  (dotest "
#+begin_src
inside the block
#+end_src
outside the block
#+end_src
")

  (dotest "
#+begin_src elisp
'lol
#+end_src
#+end_src")

  (dotest "
#+begin_src elisp
'lol
#+end_src
x wat
* 
#+end_src
")

  (dotest "
#+BEGIN_SRC elisp
'lol
#+END_SRC
x wat
#+END_SRC
")

  ; don't you love ambiguous grammars ? check out the whitespace at the end of the first #+end_src >_<
  ; when you hit 6 whitespace chars trailing on the line it splits the paragraph, woo
  (dotest "
#+begin_src elisp
'lol
#+end_src      
#+end_src")

  (dotest "
#+begin_src elisp
'lol
#+end_src

#+end_src")

  (dotest "#+begin_src")
  (dotest "#+end_src")
  (dotest "#+begin_src elisp
 (+ 1 2)
#+end_src
")

  (dotest "#+begin_src elisp\n'hello\n'there\n#+end_src")
  ; ah poop, these can be paragraphs if they are split by a newline
  ; but maybe this simplifies things
  (dotest "#+begin_src elisp\n'hello\n* OH NO A HEADLINE\n'there\n#+end_src") ;x
  (dotest "#+begin_src elisp\n* \n#+end_src") ;x

  (dotest "
#+begin_src org
,#+begin_src elisp
 (+ 1 2)
,#+end_src
#+end_src
")

  ; ok
  (dotest "\n#+begin_src l

a a:
 \"
#+end_src
")

  ; ok due to missing blank line it seems
  (dotest "#+begin_src l
b b:
 \"
#+end_src
")

  ; these are broken because blk-src doesn't match them and something in the piece-wise grammar is broken

  (dotest "#+begin_src l

c c:
 \"
#+end_src
")

  #; ; case folding is evil and should no longer be supported
  (dotest "\n#+begin_src l

d d:
 \"
#+END_SRC
")

  #; ; case folding is evil and should no longer be supported
  (dotest "\n#+BEGIN_SRC l

y y:
 \"
#+end_src
")

  (dotest " \"") ; broken
  (dotest "\n \"")

  (dotest "
#+begin_src
* oops
#+end_src
")

  )

(module+ test-big-tokes
  (current-module-path)

  (dotest "hrm :properties:")
  (dotest ":properties:")
  (dotest ":properties:asdf")
  (dotest ":properties: no newline")

  )

(module+ test-todo-kw
  (current-module-path)

  (dotest "\n* TODO something")
  (dotest "* TODO something")
  (dotest "* TODO lol COMMENT broken")

  )

(module+ test-hcom
  (current-module-path)

  (define nte 'h-comment)

  (dotest "* COMMENT")
  (dotest "* COMMENTT")
  (dotest "* COMMENTTi")
  (dotest "* COMMENT:t:") ; tag not matching correctly

  ; these are all correct, COMMENT[^ ] does NOT start a comment line
  (dotest "* COMMENTT :ARCHIVE:")
  (dotest "* COMMENTT :t:")
  (dotest "* COMMENTT :ta:")
  (dotest "* COMMENTTi :t:")
  (dotest "* COMMENTTi :ta:")

  ; the tricks that work below don't work here
  (dotest "* COMMENTT :t:")
  (dotest "* COMMENTT  :t:")
  (dotest "* COMMENTT :tag:")
  (dotest "* COMMENTTit :tag:")

  (dotest "* COMMENT ")
  (dotest "* COMMENT :t:")
  (dotest "* COMMENT :t:1:")

  ;; now fixed after reworking paragraphs to not be fallthrough
  (dotest "* COMMENT             :t:1:" #:nte nte)
  (dotest "* COMMENT          :t:1:" #:nte nte)
  (dotest "* COMMENT           :t:1:" #:nte nte)

  (dotest "* COMMENT  t                  :t:1:") ; broken, very very broken -> paragraph
  (dotest "* COMMENT   t                 :t:1:") ; XXX
  (dotest "* COMMENT      t              :t:1:") ; XXX

  (dotest "* COMMENT                     :t:1:" #:nte nte)

  (dotest "* COMMENT T" #:nte nte)
  (dotest "* COMMENT T " #:nte nte)
  (dotest "* COMMENT T  " #:nte nte)
  (dotest "* COMMENT T\n" #:nte nte)
  (dotest "* COMMENT T ARCHIVE" #:nte nte) ; just some sanity
  (dotest "* COMMENT Ti" #:nte nte)
  (dotest "* COMMENT T :ARCHIVE:")
  (dotest "* COMMENT T :t:" #:nte nte)
  (dotest "* COMMENT T :t: ")
  (dotest "* COMMENT T :t:\n")
  (dotest "* COMMENT T :ta:")
  (dotest "* COMMENT Ti :t:")
  (dotest "* COMMENT T :tag:") ; ok ok what the FOO sig going on here 7 chars total >= 2 spaces
  (dotest "* COMMENT Ti :ta:") ; ok
  (dotest "* COMMENT Tit :t:") ; ok
  (dotest "* COMMENT T   :t:") ; ok
  (dotest "* COMMENT T  :t:") ; ok
  (dotest "* COMMENT T  :ta:") ; ok
  (dotest "* COMMENT - :t:" #:nte nte)
  (dotest "* COMMENT -  :t:" #:nte nte)
  (dotest "* COMMENT T") ; ok

  ; ok now
  (dotest "* [#P]COMMENT") ;#:eq h-l1-p-c) ; FIXME BROKEN
  (dotest "* [#P] COMMENT") ; ok
  (dotest "* [#P] COMMENT:t:") ; broken
  (dotest "* [#P] COMMENT ") ; broken
  (dotest "* [#P] COMMENT :t:") ; ok
  (dotest "* [#P] COMMENT Ti") ; broken how the foo is this reading as title
  (dotest "* [#P] COMMENT T") ; broken
  ; FIXME TODO I thik we just need to prevent titles from starting with comment?
  ; can we even do that? or do we give up and go back to parsing it with comment as optional start of title?
  ; except that that is ambiguous since no-newline? includes comment ... not optional but first parse i think
  )

(module+ test-tags
  (current-module-path)

  (dotest "* H :")

  (dotest "* H :n: :t:") ; this is the counter example that proves that you cannot write a
  ; grammar for a title that will match that and not eat the last tag UNLESS you can look ahead
  ; to the newline, but we have written a newline first grammar, so we can't parse this case in
  ; a single pass unless we were to switch to newline last, and that would have unknown consequences

  (dotest "* H a:")
  (dotest "* H aa:")
  (dotest "* H 00:")

  (dotest "* H :1")

  (dotest "* H :1 :")
  (dotest "* H :1 :not_a_tag: :2 oops") ; XXX I think that this example shows that we can't do this
  (dotest "* H :t: o")
  (dotest "* H :t: oops")

  (dotest "* H :1 ")
  (dotest "* H :1 2:")
  (dotest "* H :1 2:34")
  (dotest "* H :1 2:34:")

  (dotest "* H :1 2:34::5: 6:a a:7 :tag:")
  (dotest "* H :1 :34:")

  (dotest "* H :1 2:3 4:")
  (dotest "* H :1 2: 3 4 :")
  (dotest "* H :1 2: 3 4 :")

  (dotest "* H a:1 2: 3 4 :")
  (dotest "* H a:1:2:3:45:6:")

  (dotest "* H ::")
  (dotest "* H :: ")
  (dotest "* H ::1:")
  (dotest "* H ::1: ")
  (dotest "* H :1:")
  (dotest "* H :1: ")

  ; THIS ISN'T EVEN MY FINAL FORM
  (dotest "* H :1::2: ")
  (dotest "* H :1::::::::2: ")
  (dotest "* H :1:::2::3: ")

  )

(module+ test-keywords
  (current-module-path)

  (define node-type 'keyword)
  (define nt-2 'keyword-node)
  (define p 'paragraph)
  (dotest-prefix #f)
  (dotest-prefix "\n")
  #;
  (dotest-quiet #f)

  (dotest "(" #:node-type p)
  (dotest "_" #:node-type p)
  (dotest "#+" #:node-type p)
  (dotest ":lolololol" #:node-type p)
  (dotest ":lolololol:" #:node-type p) ; malformed detached drawer

  (dotest "#+x[ ]x:" #:node-type p)
  (dotest "#+[ ]x:")
  (dotest "#+[ ] :")
  (dotest "#+x :")
  (dotest "#+k :")

  (dotest "#+]")
  (dotest "#+] ")
  (dotest "#+] :")

  (dotest "#+ :" #:nt p)
  (dotest "#+ x:" #:nt p)
  (dotest "#+x x:" #:nt p)
  (dotest "#+x x:]" #:nt p)
  (dotest "#+k [:]:" #:nt p)
  (dotest "#+x[ : ]" #:nt p)
  (dotest "#+[ :" #:nt p)
  (dotest "#+x[ :" #:nt p)
  (dotest "#+k[] :" #:nt p)
  (dotest "#+key[options] : value" #:node-type p) ; XXX paragraph missing
  (dotest "#+key[ : b] oops" #:nt p) ; XXX paragraph missing
  (dotest "#+x[ :] :" #:nt p)
  (dotest "#+x[ :]" #:nt p)
  (dotest "#+[ :]" #:nt p)

  (dotest "#+[ :]:" #:nt nt-2) ; kw

  ;HASH PLUS not-sb-colon-whitespace wsnn+ not-sb-colon-whitespace? COLON not-newline?
  ;HASH PLUS ( not-sb-colon-whitespace | RSB )+ wsnn+ not-sb-colon-whitespace? COLON not-newline?
  ;          [:                            [:
  ;HASH PLUS not-lsb-colon-whitespace wsnn+ not-lsb-colon-whitespace COLON not-newline?

  ;HASH PLUS not-colon-whitespace? LSB not-newline? wsnn+ not-newline? RSB not-colon-rsb-newline COLON not-newline? ; this is close
  (dotest "#+x[:oh :no]: OOOOOOOHHHH NOOOOOOOOOOOOOOOOOOOOOOOOOO") ; XXX ambig kw opts could be x[ or x[:oh :no]
  (dotest "#+[[[ ]x:" #:node-type p)
  ; the not-newline is the problem because it could contain another LSB? no not actually a problem at all, the LSB is actually just
  ; there for accounting purposes
  ;#+[ under what circumstances can I close this?
  ; I can close it if there is anything except for RSB before the colon IF there is a space somewhere before the close
  (dotest "#+x[[[:" #:node-type nt-2)
  (dotest "#+x[[: case]" #:node-type nt-2)
  ; if there is NOT a space before the close the what is required?
  ; t
  ;#+[]x[:
  ;#+[]x]:
  ;#+[]x[ :
  ;#+[]x] :
  (dotest "#+[]x[ ]: oof" #:node-type nt-2)

  ;#+]


  ;HASH PLUS not-lsb-colon-whitespace

  ;HASH PLUS not-colon-whitespace wsnn+ not-sb-colon-whitespace COLON
  ; #+[
  ; #+[ :]a b: and then as long as there is a whitespace before any colon we are ok

  ;HASH PLUS not-colon-whitespace wsnn+ not-sb-colon-whitespace? COLON not-newline? ; XXX wrong
  ; #+[ :]:

  ;HASH PLUS not-colon-whitespace wsnn+ not-colon-whitespace? COLON not-newline? ; XXX wrong
  ; #+[ ]:asdf
  ; but #+] :asdf

  (dotest "#+k: : :]" #:node-type nt-2)
  (dotest "#+k:k : :]" #:node-type nt-2)


  (dotest "#+[:") ; kw
  (dotest "#+k[ :]:") ; kw
  (dotest "#+k[ [:]:") ; kw

  (dotest "#+k")
  (dotest "#+k[]")

  (dotest "#+::::::::::::::::::::::::[::::::: [ ]::::::::::::]: lol")
  (dotest "#+key" #:node-type p)
  (dotest "#+key:" #:node-type nt-2)
  (dotest "#+key: value" #:node-type nt-2)
  (dotest "#+key:value" #:node-type nt-2)
  (dotest "#+ke:v" #:node-type nt-2)
  (dotest "#+k:v" #:node-type nt-2)
  (dotest "#+k:va" #:node-type nt-2)
  (dotest "#+k:va " #:node-type nt-2)
  (dotest "#+k:" #:node-type nt-2)
  (dotest "#+k: " #:node-type nt-2)
  (dotest "#+k: v" #:node-type nt-2)
  (dotest "#+k:value" #:node-type nt-2)

  (dotest "#+k[[[]:v ] ]") ; for a fun time >_< XXX FIXME SUPER ambiguous whare are empty options?
  (dotest "#+k[ [[ ]:v ] ]")
  (dotest "#+k[a[ [ ]:v ] ]") ; this could parse options as "a[ [ " OR as " [ "
  ; and I don't know if there is some underlying rule that would force one or the other
  ; because technically both are correct
  (dotest "#+k[:o :o]:v")
  #;
  (dotest "#+k[:o :o]:v :") ; broken
  (dotest "wat\n#+k[    [   [  [ []]]]]:")
  ; as an extension do we allow the empty keyword?
  (dotest "#+k[:o[nested ] :ut :oh]: big trouble") ; this one parses incorrectly an unexpctedly

  (dotest "#+[]:") ; XXX divergence this is a keyword with the empty key and empty options NOT keyword key []
  (dotest "#+:") ; XXX divergence this is a keyword with an empty key

  (dotest "#+key[" #:node-type p)
  (dotest "#+key[:")
  (dotest "#+key[asdf" #:node-type p)
  (dotest "#+key[asdf:")

  (dotest "#+key][[asdf: ]:") ; OOF now THIS is ambiguous
  #;
  (dotest "#+k[[[[lol]]]]:") ; double oof -> hyperlink ouch
  ; strongly reccomend tokenizing keyword lines independently

  (dotest "#+key[asdf: ]:")
  (dotest "#+key[asdf: I am a keyword look at me ] oops")
  (dotest "#+key[asdf: I am a keyword look at me ]: value")
  (dotest "#+key]")
  (dotest "#+key]:")

  (dotest "#+k[ :a b ]: c") ; ok
  (dotest "#+k[asdf]:") ; ok
  (dotest "#+k[ab]:") ; ok

  ; examples of why a reparse is necessary
  (dotest "#+k[a]:") ; ambig XXX but shouldn't be
  (dotest "\n#+k[a]:") ; ambig XXX but shouldn't be
  (dotest "#+k[:]:") ; grammar is ambiguous
  (dotest "#+k[: ]:") ; broken
  (dotest "#+k[:  ]:") ; broken
  (dotest "#+k[:  ]: argh") ; broken

  (dotest "#+key[]:" #:node-type 'keyword-node)
  (dotest "#+key[]" #:node-type p)
  (dotest "#+key[options]: value")
  (dotest "#+key[options]: value")
  ; FIXME lots of unintuitive behavior around here
  (dotest "#+key[: a] oops")
  (dotest "#+key[: a]: oops")
  (dotest "#+key[ : b]: value")

  #;
  (dotest-quiet #f)
  )

(module+ test-afkw
  (current-module-path)

  ; bug in the malformed-wsnn grammar incorrectly matching #+name:
  ; not clear why removing anyone of the 3 preceeding lines restores
  ; the behavior though ... the only difference I can see is that
  ; an (org-node (newline #f)) is missing in the incorrect case
  ; ...
  ; the reason is because affiliated-keyword starts with a newline now
  ; and the malformed has another preceeding newline so it eats two
  ; newlines incorrectly
  (dotest "
#+todo: TODO | DONE
#+TODO: HELLO | THERE
the above are not affiliated

#+name: wat-paragraph
wat")

  ; so, according to org-element and font locking, unaffiliated
  ; keywords are just keywords which is actually good, because it
  ; means the keyword options specifying keywords could work
  (dotest "
#+name: thing

oops not affiliated
")

  (dotest "
#+caption: oh no
#+name: thing

don't affilaite to other unaff keyword
")

  #;
  (dotest-quiet #f)
  )

(module+ test-footnotes
  (dotest "[fn::]")
  (dotest "[fn::")


  (dotest "[fn::a][fn::b]")

  ; nested
  (dotest "[fn::a[fn::b]]")

  ; deeply nested
  (dotest "[fn::[fn::[fn::[fn::[fn::[fn::]]]]]]")

  (dotest "[fn:def] always a definition")

  ; cursed
  (dotest "[fn::=]=]")
  (dotest "[fn:: =]= ]")
  (dotest "[fn:: x =]= y ]")
  (dotest "[fn::=[=]") ; XXX
  (dotest "=[= [fn:: x =]= y ]")  ; FIXME HRM this is mismatched do footnotes somehow take priority over verbatim !?
  (dotest "[fn:: [ x =]= y ]")
  (dotest "[fn:: =[= x ] y ]")
  (dotest "[fn:: =[= x =]= y ]") ; FIXME bad markup

  ; cursed anchor
  (dotest "[fn:lol]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]")

  (dotest "Text [fn::Inline footnote[fn::Nested.].] more." #:expand? #t)

  (dotest "be[fn::in[fn::ne]]aft") ; FIXME bad parse on the ne

  (dotest "a[fn::b[fn:c]]")


  (dotest "[fn:: sigh\nwhat")
  (dotest "[fn:: sigh\n\nwhat")

  (dotest "[fn:: asdf")

  (dotest "be[fn::in[fn::ne]
#+begin_src bash
echo oops a block
#+end_src
]aft") ; XXX broken because the inline footnote doesn't know to stop for blocks

  (dotest "Text [fn::Inline footnote.] more.")
  (dotest "Text [fn::Inline footnote.\nmore footnote.] more.") ; yes

  (dotest "Text [fn::Inline footnote.\n\nA new paragraph?] more.") ; officially NOT an inline footnote YAY!!!!!!!

  (dotest "Text [fn:anchor] more.")
  (dotest "Text [fn:x] more.\n[fn:x] Gonna")

  (dotest "A [fn:1].\n[fn:1] footnote.\n**\n")
  (dotest "A [fn:1].\n[fn:1] footnote.\n** \n")

  ; FIXME none of these should be inline ! which means that we have to update the paragraph tokenizer to skip ^[fn:
  (dotest "[fn::]")

  (dotest "[fn:: hello =]= there ]")

  (dotest "[fn:: [ hello =]= there ]")

  (dotest "[fn:: [ hello =]= there\n\n\nanother p]")

  (dotest "[fn:: =a= \n\n")

  (dotest "[fn:: =a= \n")

  (dotest "[fn:: =a=\n")

  (dotest "[fn:: =a= \n\n b]")

  (dotest "[fn:: =a= \n b]") ; XXX verbatim here is wrong as well ... off by two error

  )

(module+ test-cite
  ; from test-org-element
  (dotest "[cite:@key]" #:nte 'citation)
  (dotest "[cite:-@key]" #:nte 'citation)
  (dotest "[cite:text]") ; not key
  (dotest "[cite/style:@key]" #:nte 'citation)
  #; ; TODO ?
  (dotest "[cite/elyts:@key]" #:nte 'style) ; element property should be style
  (dotest "[cite:@a;@b;@c]" #:nte 'citation)
  #; ; TODO not sure of the distinction
  (dotest "[cite:@a;@b;@c]" #:nte 'citation-reference)
  (dotest "[cite:@a;-@b]")
  (dotest "[cite:common-prefix@a]") ; ??? what should this be
  (dotest "[cite:common-prefix;@a]")
  (dotest "[cite:@a;common-suffix]")
  (dotest "[cite: common-prefix ;@a]")
  (dotest "[cite: @a; common-suffix]")
  (dotest "[cite: @a ; common-suffix]") ; ???
  (dotest "[cite:common-prefix;@a;common-suffix]")
  (dotest "[cite:@[]]") ; not key
  (dotest "[cite:@a:.#$%&-+?<>~/1]" #:nte 'citation)
  (dotest "[cite:@;]") ; not citation-reference ? XXX

  (dotest "[cite: pre @key post]")
  (dotest "[cite: pre @key]")
  (dotest "[cite:@key post]")

  (dotest "[cite: pre1 @k1 post1; pre2 @k2 post2]")
  (dotest "[cite: pre1 @k1;pre2 @k2 post2]")
  (dotest "[cite: pre1 @k1 post1;@k2 post2]")
  (dotest "[cite:@k1 post1; pre2 @k2]")

  )

(define h-l1
  '(org-file
    (headline-node (heading 1 (tags)))))
(define h-l1-c 
  '(org-file
    (headline-node
     (headline
      (stars "*")
      (headline-content (h-sane-comment (h-comment "COMMENT")))))))
(define h-l1-t
  '(org-file
    (headline-node
     (headline
      (stars "*")
      (headline-content (h-tags (tag-name "t") (tag-name "1")))))))
(define h-l1-p
  '(org-file
    (headline-node
     (headline
      (stars "*")
      (headline-content (h-priority (priority-level (not-whitespace1 "P"))))))))

(module+ test-sentinel
  (current-module-path)
  (laundry-tokenizer-debug #f)

  ; things that work and should continue to work

  (dotest "99." #:nte 'ordered-list-line) ; -> ordered list

  (dotest "p 1
# c 1
# c 2
p 2
" #:nte 'comment-element)

  (dotest "#+begin_x
* h
#+end_x" #:nte 'heading)


  (dotest "|\nxx" #:nte 'table)
  (dotest "|\nxx" #:nte 'paragraph)

  (dotest "* ")
  (dotest "* " #:eq-root h-l1)
  (dotest "*  " #:eq-root h-l1)
  (dotest "*                                     ")
  (dotest "* COMMENT" #:nte 'comment)
  (dotest "* COMMENT " #:nte 'comment)
  (dotest "* COMMENT[#A]")

  (dotest "* ::" #:nte 'tags)
  (dotest "* :: " #:nte 'tags)
  (dotest "* :::" #:nte 'tags)
  (dotest "* ::: " #:nte 'tags)
  (dotest "* title ::: " #:nte 'tags)
  (dotest "* title ::" #:nte 'tags)
  (dotest "* title ::t:" #:nte 'tags)
  (dotest "* :t:1:")
  (dotest "* :t:1: ")
  (dotest "* [#P]" #:nte 'h-priority)
  (dotest "* [#P] " #:nte 'h-priority)

  ; FIXME TODO deal with newlines preceeding the start of the file
  #;
  (dotest "" #:eq     '(org-file (org-node (bof))))
  (dotest "" #:eq-root '(org-file (empty-line "\n")))
  #;
  (dotest "\n" #:eq   '(org-file (org-node (bof)) (org-node (newline #f))))
  (dotest "\n" #:eq-root '(org-file (empty-line "\n") (empty-line "\n")))
  #;
  (dotest "  " #:eq   '(org-file (org-node (paragraph (space #f) (space #f)))))
  (dotest "  " #:eq-root   '(org-file (paragraph "\n" "  "))) ; FIXME vs "\n  "
  (dotest "  \n" #:eq-root '(org-file (paragraph "\n" "  ") ; FIXME vs "\n  "
                                      (empty-line "\n")))

  (dotest "* :tag:")
  (dotest "*   :ARCHIVE:")
  (dotest "*   :ARCHIVE:t:")
  (dotest "* :t:ARCHIVE:")
  (dotest "* :t:ARCHIVE:1:")

  (dotest "* this is an example of :ARCHIVE: being used inline and :in:a:ARCHIVE:tag:")

  (dotest "* :t:ARCHIVE:!:") ; BANG not tags

  (dotest "* :t:1:@:#:_:%:")
  (dotest "*   :!asdft1@#_%:       ")
  (dotest "*   :asdft1@#_%:       ")

  (dotest "* :a: b:cd:")
  (dotest "* :a: b:cd:")
  (dotest "* :a: b:cd:")
  (dotest "* :a: b:cd:")
  (dotest "* :a:!b:cd:")
  (dotest "* :a:-b:cd:")

  ; TODO iirc plain list lines probably also need their own subgrammar
  (dotest "- [@9")
  (dotest "- [@9]")
  (dotest "- [@999")
  (dotest "- [@999]")

  )

(module+ test-word-char-vs-word-char-n
  (current-module-path)

  (dotest "* Heading\n  lower case is ok")

  (dotest "* Heading\n  oh Ok")
  (dotest "* Heading\noK")
  (dotest "* Heading\nHa")
  (dotest "* \na")
  (dotest "* \nA")

  (dotest "* \n ab")

  (dotest "* \n a")
  (dotest "* \n Aa")
  (dotest "* \n\taA")

  (dotest "* Heading\n  Ab cD")
  (dotest "* Heading\n  Ab ")
  
  (dotest "* Heading\n  a|")

  (dotest "* Heading\n  aB1d")
  (dotest "#lang org")
  (dotest "#a c d")
  (dotest " #a c d")
  (dotest "#ab c d")
  (dotest " #ab c d")

  (dotest "#+ a")
  (dotest "# a")
  (dotest ". you can totally start a line with a period")
  (dotest ") you can totally start a line with a right paren")

  (dotest "* Heading\n  Leading whitspace followed by an uppercase is not")
  )

(module+ test-files
  (current-module-path)

  #; ; #+CAPTION: Value bug ...
  (define setup (dotest-file "~/git/sparc-curation/docs/setup.org"))
  ; yeah ... for big files scanning to heading offsets and parsing
  ; headings in parallel to avoid the quadratic behavior is likely to
  ; be a reasonable workaround for big files as we develop the
  ; implementation, because the slowdown is extremely debilitating

  ; this takes WAY too long to parse it is _abysmally_ slow !??!?!
  ; it is just plain text wat wat wat !??!?!
  ; leading whitespace irrelevant wrt performance
  ; there is some quadratic nonsense going on in there
  ; you can tell because simple runs very quickly
  ; but notes which is only a bit longer runs very very slowly
  ; nasty quadratic behavior when parsing paragraphs with normal prose due to short token quadratic issues
  ;; XXX a workaround for the quadraticness has been acomplished by parsing the 80% case for paragraphs
  ;; via the tokenizer
  (define notes (dotest-file "~/git/sparc-curation/docs/notes.org"))

  ; OOF this one still hits the quatratic behavior very hard
  ; WOW it actually parses ...
  (define apinatomy (dotest-file "~/git/sparc-curation/docs/apinatomy.org"))

  (define alt (dotest-file "~/git/interlex/alt/README.org"))

  (define rel (dotest-file "~/git/pyontutils/docs/release.org")) ; x

  (define scig (dotest-file "~/git/pyontutils/nifstd/scigraph/README.org")) ; works, but slow, possibly due to big src blocks

  (dotest "
wat
#+begin_src bash
#+end_src
")


  (dotest "\nasdf asdf as3 fasd fasd f\nlololo1 lo2ol l l l l l l oooo ol\n")

  ; aff key eof issues
  (dotest "#+header: :var a=1\n")
  (dotest "\n#+header: :var a=1")
  (dotest "#+header: :var a=1") ; FIXME quite broken this should be a keyword !?

  (dotest "#+keyword: value")

  (dotest "\na 2 c\nd 5 f\n,")
  (dotest "a\n,")
  (dotest "\na\n,")

  (define simple (dotest-file "simple.org"))
  (dotest-file "simple.org")

  (define test-org (dotest-file "test.org")) ; WOOOOOOOOOOOOOOO all working ?! yay for tokenizers simplifying things?

  (define test-pos-vs-loc-org (dotest-file "test-pos-vs-loc.org"))

  #; ; #+results[a]: issue
  (define dev-guide (dotest-file "~/git/sparc-curation/docs/developer-guide.org"))

  (dotest "
#+RESULTS[99f2d5eba83acdff6934d4fdfe64da4170348550]:
") ; x

  (dotest "***nasdf")
  (dotest "***n")

  (dotest "
#+begin_src wat
#+end_src")

  (dotest "
#+begin_src wat
a
#+end_src")

  )

(module+ test-not-markup
  (current-module-path)
  (laundry-tokenizer-debug #f)

  (dotest "+wat" #:node-type 'paragraph)
  (dotest " +wat" #:node-type 'paragraph)

  (dotest " *wat" #:node-type 'paragraph)
  (dotest "*wat" #:node-type 'paragraph)
  (dotest "*wat\n" #:node-type 'paragraph)

  (dotest " q:wat" #:node-type 'paragraph)
  (dotest " q :wat" #:node-type 'paragraph)
  (dotest " wat:" #:node-type 'paragraph)
  (dotest " :wat" #:node-type 'paragraph)
  (dotest " :w" #:node-type 'paragraph)

  (dotest " * wat" #:node-type 'descriptive-list-line)
  (dotest "* wat" #:node-type 'headline)
  (dotest " wat" #:node-type 'paragraph)


  (dotest "_wat")
  (dotest "=wat")
  )

(module+ test-known-broken
  (current-module-path)

  ; this goes last so we can sort out remaining issues

  ; from test-paragraph-start
  (dotest "#+end ")
  (dotest "#+end-")

  )

(module+ test
  (require
   (submod ".." test-sentinel)
   (submod ".." test-headline-content)
   (submod ".." test-bof)
   (submod ".." test-wat)
   (submod ".." test-list)
   (submod ".." test-npnn)
   (submod ".." test-cell)
   (submod ".." test-row)
   (submod ".." test-table) ; XXX
   (submod ".." test-planning)
   (submod ".." test-paragraph-start)
   (submod ".." test-paragraphs)
   (submod ".." test-markup)
   (submod ".." test-macros)
   (submod ".." test-comments) ; XXX
   (submod ".." test-rando)
   (submod ".." test-drawers)
   (submod ".." test-string)
   #; ; see note at module
   (submod ".." test-switches)
   (submod ".." test-block-switches)
   (submod ".." test-blk-dyn)
   (submod ".." test-blocks)
   (submod ".." test-big-tokes)
   (submod ".." test-todo-kw)
   (submod ".." test-hcom)
   (submod ".." test-tags)
   (submod ".." test-keywords)
   (submod ".." test-afkw)
   (submod ".." test-footnotes)
   (submod ".." test-cite)
   (submod ".." test-word-char-vs-word-char-n)
   (submod ".." test-not-markup)
   #; ; XXX big boy
   (submod ".." test-files)
   #; ; known broken
   (submod ".." test-known-broken)
   ))
