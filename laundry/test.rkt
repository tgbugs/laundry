#lang racket/base

(require racket/list
         racket/file
         racket/path
         racket/pretty
         racket/string
         laundry/parser
         laundry/tokenizer
         (rename-in (only-in laundry/heading make-rule-parser)
                    [make-rule-parser heading-rule-parser]))

(provide dotest dotest-q dotest-file dotest-quiet)
(define testing-parse (make-parameter parse))
(define testing-token (make-parameter laundry-make-tokenizer))
(define dotest-prefix (make-parameter #f))
(define dotest-suffix (make-parameter #f))
(define dotest-quiet (make-parameter #t)) ; parameters across require ... ?
#;
(define testing-parse-to-datum (make-parameter parse-to-datum))
(define (rec-cont tree atom)
  ; I'm lazy and not going to write a proper bfs
  (string-contains? (pretty-format tree) (symbol->string atom)))


(define-namespace-anchor anc-test)
(define ns-test (namespace-anchor->namespace anc-test))
(define (dotest test-value #:eq [eq #f] #:eq-root [eq-root #f]
                #:node-type [node-type #f] #:quiet [quiet #f]
                #:parse-to-datum [parse-to-datum #f]
                ; FIXME parameterize do-expand (define test-expand (make-parameter #t))
                #:expand? [do-expand #t])
  (define test-value-inner
    (let ([p (dotest-prefix)]
          [s (dotest-suffix)])
      (let ([prepended (if p (string-append p test-value) test-value)])
        (if s (string-append prepended s) prepended))))
  (define (t) ((testing-token) (open-input-string test-value-inner)))
  (define hrm ((if parse-to-datum
                   parse-to-datum
                   (compose syntax->datum (testing-parse)))
               (t)))
  (when (and node-type (not (rec-cont hrm node-type)))
    (error (format "parse does not contain ~s" node-type)))
  (when (and eq (not (equal? eq hrm)))
    (error (format "foo ~s ~s" test-value-inner hrm)))
  ; WAIT!? setting a parameter modifies the parent in module+ but not
  ; if you actually require the module !? ARGH!
  (unless (or quiet (dotest-quiet))
    (pretty-write (list 'dotest: hrm)))
  (if do-expand
    (when do-expand ; LOL when x vs begin ...
      (define hrms #`(module org-module laundry/expander
                       #,((testing-parse) ; watch out for the 2 arity case in the case-lambda here
                          #; ; NAH just a completely insane case-lambda
                          ; that causes the call to revert to the full grammar
                          (format "test-source ~s" test-value-inner)
                          (t))))
      (parameterize ([current-namespace ns-test])
        (eval-syntax hrms)
        (define root (dynamic-require ''org-module 'root))
        (when (and eq-root (not (equal? eq-root root)))
          (error (format "foo ~s ~s" test-value-inner root)))
        (unless (or eq-root node-type)
          root)))
    (unless (or eq node-type)
      hrm)))

(require debug/repl)
(define (dotest-q test-value)
  (dotest test-value #:quiet #t))

(define (dotest-fail test-value)
  (unless (with-handlers ([exn? (λ (exn) #t)])
            (dotest test-value)
            #f)
    (error "Should have failed.")))

(define (dotest-file path #:eq [eq #f])
  ; FIXME super inefficient
  #;
  (define (t) (laundry-make-tokenizer (open-input-string (file->string (string->path path) #:mode 'text))))
  (define hrm
    (with-input-from-file (expand-user-path (string->path path))
      (λ ()
        (let ([t (laundry-make-tokenizer (current-input-port))])
          (parse-to-datum t)))
      #:mode 'text))
  (if eq
      (unless (equal? eq hrm)
        (error (format "path bar ~s" path)))
      #;
      (pretty-print hrm)
      hrm))

(module+ test-bof
  ; fooing annoying as foo having to duplicate the whole fooing grammar
  ; just for this one fooing little special case FOO
  (dotest "")
  (dotest " ")
  (dotest " \n")
  (dotest "* ")
  (dotest "* Headline")
  (dotest "* Headline\n:properties:\n:poop: foo\n:end:") ; FIXME broken again
  (dotest "Paragraph foo poop.")
  (dotest "1. ordered list")
  (dotest "- descriptive list")
  (dotest "| table ")
  (dotest ":drawer:\n:end:")
  )

(module+ test-list
  (define node-type 'plain-list-line)
  (dotest "0." #:node-type node-type)
  (dotest "0)" #:node-type node-type)
  (dotest "0. " #:node-type node-type)
  (dotest "0.  " #:node-type node-type)
  (dotest " * poop" #:node-type node-type)
  (dotest " - foo" #:node-type node-type)

  (dotest "A. " #:node-type node-type)
  (dotest "A. asdf" #:node-type node-type)
  (dotest "a. " #:node-type node-type)

  (dotest "1. \n 2. " #:node-type node-type)

  (dotest " A. asdf" #:node-type node-type)
  (dotest " a. " #:node-type node-type)
  (dotest "1. \n a. " #:node-type node-type)
  (dotest "1. There\n a. asdf" #:node-type node-type)
  (dotest "1. There\n A. asdf" #:node-type node-type)
  (dotest "1. There\n A. asdf\n" #:node-type node-type)

  )

(module+ test-npnn
  (dotest "asdf" #:node-type 'paragraph)
  (dotest ";alksdjf;l jd; j;1oj;oij10j [p0j asd;foja ;kjas.d/f a.ldfjaoiejf01923jOAJ--1!@@#$%^&*[]{}\\/" #:node-type 'paragraph)
  (dotest "\n")
  (dotest "|" #:node-type 'table)
  )

(module+ test-cell
  ; from this it seems that we can't do PIPE? at the end because it will be eaten
  ; nope, not true, the PIPE? takes precednece so that does work in the limited case
  (dotest "|" #:eq '(org-file (org-node (table (table-row (table-cell))))))
  (dotest "||" #:eq '(org-file (org-node (table (table-row (table-cell))))))
  (dotest "|||" #:eq '(org-file (org-node (table (table-row (table-cell) (table-cell))))))
  (dotest "||||")
  (dotest "|||||")
  (dotest "|||||||||||||||||||||||||||||||||||||")
  (dotest "||||||||||||||||||||||||||||||||||||||")

  (dotest "|a")
  (dotest "|a|")
  (dotest "|a||")
  (dotest "||a|")
  (dotest "||a|b") ;
  (dotest "||a|b|")

  (dotest "|||oops|||oops|||oops oops|||")
  (dotest "|a\n") ;
  (dotest "| hello there")

  (dotest "|a ")
  (dotest "\n|a")

  (dotest "| a | yo") ;

  (dotest "| a | yo |")

  )

(module+ test-row
  (dotest "|\n|" #:eq '(org-file (org-node (table (table-row (table-cell)) (table-row (table-cell))))))
  (dotest "||\n|")
  (dotest "||\n||")
  (dotest "|a|\n||")
  (dotest "|a|\n|c|")
  (dotest "|a|b\n|c|")
  (dotest "|a|b\n|c|d")
  (dotest "|a|b|\n|c|d|")

  )

(module+ test-table
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
  (dotest "|")
  (dotest " |")
  (dotest "|\n")
  (dotest "|\n|")
  (dotest "|\n|\n")
  (dotest "|\n|\n||")
  (dotest "|\n|\n|||")
  (dotest "|\n|\n|||\n|")
  (dotest "||")
  (dotest "||\n||")
  (dotest "\n| the foo")
  ; FIXME the bug here is that these should all be one table but show up as multiple tables
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

  )

(module+ test-headline-content

  (parameterize ([testing-parse
                  ; XXX watch out for the 2 arg part of case-lambda produced by make-rule-parser
                  (heading-rule-parser --test--heading-rest)]
                 [testing-token (bind-runtime-todo-keywords)]
                 [dotest-prefix " "]
                 [dotest-suffix "\n"])
    (dotest "")
    (dotest ":")
    (dotest ": ")
    (dotest ":t:")
    (dotest ":n: :t:") ;
    (dotest "TODO")
    (dotest "DONE")

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

(module+ test-headline
  ; FIXME not sure what COMMENT breaks things ?!
  ;(define test-value "this is ORG MODE\n* headline\n YEAH\n** COMMENT comment headline\nstuff\n* hl2 :tag:\n")
  ; ok, so BOF and EOF are causing annoying edge cases
  #;
  (dotest-quiet #f)

  (dotest "* " #:node-type 'headline)
  (dotest (string-append (make-string 99 #\*) " "))
  (dotest "* H")
  (dotest "* H ") ; how the foo does this work
  (dotest "* He") ; and this work
  (dotest "* He ") ; but this fail?


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

  ; the behavior of org-export is clear here
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
  (dotest "** Headline level 2\n")

  (dotest "why can't we share this newline?\n** Headline level 2\n") ; FIXME why is this so slow to parse

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

  )

(module+ test-paragraph-start

  (dotest "  #+begin_src")
  (dotest "  #+begin_srclol" #:node-type 'bg-type-special)
  (dotest "  #+begin_src\n")
  (dotest "  #+begin_")
  (dotest "  #+begin_-") ; -> block
  (dotest "  #+begin_:") ; -> block

  (dotest "#+end:" #:node-type 'keyword) ; -> keyword
  (dotest "  #+end:" #:node-type 'keyword)

  (dotest "  #+end")
  (dotest "#+end")
  (dotest "  #+end_")
  (dotest "#+end_")
  (dotest "#+end_srclol" #:node-type 'bg-end-special) ; -> block

  (dotest "#+:end::properties::end: lol" #:node-type 'keyword) ; -> keyword
  (dotest "#+:end:" #:node-type 'keyword) ; -> keyword
  (dotest "#+:properties:" #:node-type 'keyword) ; -> keyword
  (dotest "#+:end::asdf" #:node-type 'paragraph) ; XXX inconsistnet org element says keyword, font locking and spec say paragraph


  (dotest (make-string 99 #\*))
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

  (dotest "  #" #:node-type 'comment) ; -> comment
  (dotest "#" #:node-type 'comment) ; -> comment
  (dotest "# " #:node-type 'comment) ; -> comment
  (dotest "  #a" #:node-type 'paragraph)
  (dotest "#a" #:node-type 'paragraph)
  (dotest "  #+")
  (dotest "#+")
  (dotest "#+:")
  (dotest "#+:aaaa:" #:eq-root '(org-file (org-node (keyword (keyword-key ":aaaa")))))
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
  (dotest "  9999999.") ; -> ordered list
  (dotest "9999999.") ; -> ordered list
  (dotest "  9999999.a")
  (dotest "9999999.a")

  (dotest "  COMMENT")
  (dotest "COMMENT")
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
  (dotest "aaaaaaaaa paragraph")

  (dotest "\nIf nothing else this is a paragraph.")
  ; (dotest "p") ; sigh bof
  (dotest "\np")
  (dotest "\np\n")
  (dotest "\np a r a g p\n")

  ; what the heck ; resolved now, issue identified see note at top of file in impl approach and no backtracking optimization
  ; (dotest "p a r a g p") ; ok 
  (dotest "\np a r a g p") ; totally broken !??!?!?!
  (dotest "\naaaaaaaaaaa") ; also totally broken ... wat
  ;(dotest "aaaaaaaaaaa")
  ;(dotest "aaaaaaaaaaaa")
  (dotest "\n0123456789") ; ok
  (dotest "\n0123456789A") ; broken
  (dotest "\n01234567890") ; broken
  (dotest "\n0123456789\n") ; ok
  (dotest "\n0123456789A\n") ; broken
  #; ; this is so fooing slow that I'm commenting it out what the foo
  (dotest
   ; FIXME this takes an astoundlingly long time to parse if paragraph-line : @not-newline+
   ; it takes a stupidly long time even if it is just defined as not-newline ... wtf
   ; it totally poops the bed with the newline but is just fine without it ...
   "\nI'm sorry I really just do not understand what the issue is here.") 

  (dotest
   ; and this one runs just fine ... sort of, still slower than desired
   "\nI'm sorry I really just do not understand what the issue is here.")

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
  (dotest "p") ; broken again due to BOF issues
  (dotest "\n\np\n\n")
  (dotest "\n0123456789A")
  (dotest "0123456789A") ; broken BOF
  (dotest "paragraph\n") ; broken BOF

  )

(module+ test-markup

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
  (dotest "/hahaha/") ; XXX bof issues in the tokenizer
  (dotest " /asdf/")
  (dotest "\n/asdf/")
  (dotest "\n**oops\n* hello\nOH NO *lol*")

  (dotest "*/bi/*")
  (dotest "/*ib*/")
  (dotest "/*_ibu_*/")
  (dotest "_*/ubi/*_")

  (dotest "*/_+bius+_/*")

  (dotest "*bold text /bi text/ bold _bu text_ bold =bold verb= bold ~bold code~ code*") ; NOOOOOOOOOOOOOO

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

  )

(module+ test-macros

  (dotest "{{{macro}}}")
  (dotest "{{{macro(a,b,c)}}}")

  )

(module+ test-comments

  (dotest "35934")
  (dotest "35934" #:eq-root '(org-file (org-node (paragraph "\n35934")))) ; FIXME bof newline ...
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
")

  (dotest "\n#+NAME: lol\nare you fooing kidding me!")

  )

(module+ test-drawers
  (dotest "
* Headline 1
:properties:
:olo: lol
:end:" #:node-type 'property-drawer)

  #; ; correctly broken
  (dotest "
* asdf
:properties:
:hrm: oops :end:")
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

  (dotest "\n:drawer:\n\n:end:")

  (dotest "\n:drawer:\n:end:")

  (dotest ":d:\n:end:")
  (dotest ":d:\n* \n:end:") ; foo yeah it works ; FIXME malformed ?

  (dotest ":ARCHIVE:\n:end:")

  )

(module+ test-string
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

  ; for some reason this one is slighly bugged
  (dotest "
#+begin_src elisp
'lol
#+end_src
#+end_src")

  ; don't you love ambiguous grammars ? check out the whitespace at the end of the first #+end_src >_<
  ; when you hit 6 whitespace chars trailing on the line it splits the paragraph, woo
  (dotest "
#+begin_src elisp
'lol
#+end_src      
#+end_src")

  ; and this one parses correctly for reasons not currently understood
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

  )

(module+ test-big-tokes
  (dotest "hrm :properties:")

  )

(module+ test-todo-kw
  
  (dotest "\n* TODO something")
  (dotest "* TODO something")
  (dotest "* TODO lol COMMENT broken")

  )

(module+ test-hcom

  (dotest "* COMMENT")
  (dotest "* COMMENTT")
  (dotest "* COMMENTTi")
  (dotest "* COMMENT:t:") ; tag not matching correctly

  (dotest "* COMMENTT :ARCHIVE:")
  (dotest "* COMMENTT :t:") ; BROKEN
  (dotest "* COMMENTT :ta:") ; BROKEN
  (dotest "* COMMENTTi :t:") ; BROKEN
  (dotest "* COMMENTTi :ta:") ; BROKEN

  ; the tricks that work below don't work here
  (dotest "* COMMENTT :t:") ; BROKEN
  (dotest "* COMMENTT  :t:") ; BROKEN
  (dotest "* COMMENTT :tag:") ; BROKEN
  (dotest "* COMMENTTit :tag:") ; BROKEN

  (dotest "* COMMENT ")
  (dotest "* COMMENT :t:")
  (dotest "* COMMENT :t:1:") ; BROKEN

  ;; now fixed after reworking paragraphs to not be fallthrough
  (dotest "* COMMENT             :t:1:") ; broken
  ;;;;;;;
  (dotest "* COMMENT          :t:1:") ; ok ?
  (dotest "* COMMENT           :t:1:") ; broken
  (dotest "* COMMENT  t                  :t:1:") ; broken, very very broken -> paragraph
  (dotest "* COMMENT   t                 :t:1:") ; ok
  ;;;;;;;
  (dotest "* COMMENT      t              :t:1:") ; ok
  (dotest "* COMMENT                     :t:1:") ; BROKEN WHAT THE FOO -> paragraph

  (dotest "* COMMENT T")
  (dotest "* COMMENT T ")
  (dotest "* COMMENT T  ")
  (dotest "* COMMENT T\n")
  (dotest "* COMMENT T ARCHIVE") ; just some sanity
  (dotest "* COMMENT Ti")
  (dotest "* COMMENT T :ARCHIVE:")
  (dotest "* COMMENT T :t:") ; BROKEN
  (dotest "* COMMENT T :t: ") ; BROKEN
  (dotest "* COMMENT T :t:\n") ; BROKEN
  (dotest "* COMMENT T :ta:") ; BROKEN
  (dotest "* COMMENT Ti :t:") ; BROKEN
  (dotest "* COMMENT T :tag:") ; ok ok what the FOO sig going on here 7 chars total >= 2 spaces
  (dotest "* COMMENT Ti :ta:") ; ok
  (dotest "* COMMENT Tit :t:") ; ok
  (dotest "* COMMENT T   :t:") ; ok
  (dotest "* COMMENT T  :t:") ; ok
  (dotest "* COMMENT T  :ta:") ; ok
  (dotest "* COMMENT - :t:") ; BROKEN
  (dotest "* COMMENT -  :t:") ; BROKEN
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
  (define node-type 'keyword)
  (dotest "#+key")
  (dotest "#+key:")
  (dotest "#+key: value")

  (dotest "#+key[")
  (dotest "#+key[:")
  (dotest "#+key[asdf")
  (dotest "#+key[asdf:")

  (dotest "#+key[asdf: ]:")
  (dotest "#+key[asdf: I am a keyword look at me ] oops")
  (dotest "#+key[asdf: I am a keyword look at me ]: value")

  (dotest "#+key]")
  (dotest "#+key]:")

  (dotest "#+key[]:" #:node-type node-type)
  (dotest "#+key[]")
  (dotest "#+key[options]: value")
  (dotest "#+key[options] : value")
  (dotest "#+key[options]: value")
  ; FIXME lots of unintuitive behavior around here
  (dotest "#+key[: a] oops")
  (dotest "#+key[: a]: oops")
  (dotest "#+key[ : b] oops")
  (dotest "#+key[ : b]: value")

  )

(define h-l1
  '(org-file
    (org-node (headline-node (heading 1 ())))))
(define h-l1-c 
  '(org-file
    (org-node
     (headline-node
      (headline
       (stars "*")
       (headline-content (h-sane-comment (h-comment "COMMENT"))))))))
(define h-l1-t
  '(org-file
    (org-node
     (headline-node
      (headline
       (stars "*")
       (headline-content (h-tags (tag-name "t") (tag-name "1"))))))))
(define h-l1-p
  '(org-file
    (org-node
     (headline-node
      (headline
       (stars "*")
       (headline-content (h-priority (priority-level (not-whitespace1 "P")))))))))

(module+ test-sentinel
  ; things that work and should continue to work

  (dotest "* ")
  (dotest "* " #:eq-root h-l1)
  (dotest "*  " #:eq-root h-l1)
  (dotest "*                                     ")
  (dotest "* COMMENT")
  (dotest "* COMMENT ")
  (dotest "* ::" ); #:eq h-l1-t)
  (dotest "* :: " ); #:eq h-l1-t)
  (dotest "* :::" ); #:eq h-l1-t)
  (dotest "* ::: " ); #:eq h-l1-t)
  (dotest "* :t:1:"); #:eq h-l1-t)
  (dotest "* :t:1: "); #:eq h-l1-t)
  (dotest "* [#P]" );#:eq h-l1-p) ; foo
  (dotest "* [#P] " ); #:eq h-l1-p) ; foo

  #;
  (dotest "" #:eq     '(org-file (org-node (bof))))
  (dotest "" #:eq-root '(org-file (org-node "\n")))
  #;
  (dotest "\n" #:eq   '(org-file (org-node (bof)) (org-node (newline #f))))
  (dotest "\n" #:eq-root '(org-file (org-node "\n") (org-node "\n")))
  #;
  (dotest "  " #:eq   '(org-file (org-node (paragraph (space #f) (space #f)))))
  (dotest "  " #:eq-root   '(org-file (org-node (paragraph "\n  "))))
  (dotest "  \n" #:eq-root '(org-file (org-node (paragraph "\n  "))
                                      (org-node "\n")))

  (dotest "* :tag:")
  (dotest "*   :ARCHIVE:")
  (dotest "*   :ARCHIVE:t:")
  (dotest "* :t:ARCHIVE:")
  (dotest "* :t:ARCHIVE:1:")

  (dotest "* this is an example of :ARCHIVE: being used inline and :in:a:ARCHIVE:tag:")

  (dotest "* :t:ARCHIVE:!:") ; BANG not tags

  (dotest "* :t:1:@:#:_:%:")
  (dotest "*   :!asdft1@#_%:       ")

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
  #; ; #+CAPTION: Value bug ...
  (define setup (dotest-file "~/git/sparc-curation/docs/setup.org"))

  ; this takes WAY too long to parse it is _abysmally_ slow !??!?!
  ; it is just plain text wat wat wat !??!?!
  ; leading whitespace irrelevant wrt performance
  ; there is some quadratic nonsense going on in there
  ; you can tell because simple runs very quickly
  ; but notes which is only a bit longer runs very very slowly
  ; nasty quadratic behavior when parsing paragraphs with normal prose due to short token quadratic issues
  ;; XXX a workaround for the quadraticness has been acomplished by parsing the 80% case for paragraphs
  ;; via the tokenizer
  (define notes (dotest-file "~git/sparc-curation/docs/notes.org"))

  ; OOF this one still hits the quatratic behavior very hard
  ; WOW it actually parses ...
  (define apinatomy (dotest-file "~/git/sparc-curation/docs/apinatomy.org"))

  (define alt (dotest-file "~/git/interlex/alt/README.org"))

  (define rel (dotest-file "~/git/pyontutils/docs/release.org")) ; x

  (define scig (dotest-file "~/git/pyontutils/nifstd/scigraph/README.org")) ; x

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
  ; this goes last so we can sort out remaining issues

  ; from test-paragraph-start
  (dotest "9)ARCHIVE")
  (dotest "#+end ")
  (dotest "#+end-")
  (dotest "COMMENT more")

  (dotest "#+end_ " #:node-type 'malformed) ; ok
  (dotest "  #+begin_src oops not a thing") ; ok
  (dotest "  #+begin_ more" #:node-type 'paragraph) ; ok

  ; from test-drawers
  (dotest "
:random-drawer:

you called?
@@hrm: oops @@ in drawer context esinips are ignored
   :end:

:end:
") ; ok

  ; from test-big-tokes
  (dotest ":properties:")
  (dotest ":properties:asdf")
  (dotest ":properties: no newline")

  )

(module+ test
  (require
   (submod ".." test-headline-content)
   (submod ".." test-bof)
   (submod ".." test-list)
   (submod ".." test-npnn)
   (submod ".." test-cell)
   (submod ".." test-row)
   (submod ".." test-table)
   (submod ".." test-planning)
   (submod ".." test-paragraph-start)
   (submod ".." test-paragraphs)
   (submod ".." test-markup)
   (submod ".." test-macros)
   (submod ".." test-comments)
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
   (submod ".." test-sentinel)
   (submod ".." test-word-char-vs-word-char-n)
   (submod ".." test-not-markup)
   #; ; XXX big boy
   (submod ".." test-files)
   #; ; known broken
   (submod ".." test-known-broken)
   ))
