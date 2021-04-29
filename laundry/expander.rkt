#lang racket/base
(require
 racket/stxparam
 (only-in racket/function identity)
 (only-in racket/list splitf-at)
 (for-meta 2
           racket/base
           syntax/parse
           )
 racket/pretty
 (only-in laundry/parser make-rule-parser)
 ;(only-in laundry/tokenizer ) ; FIXME -> syntax time
 (for-syntax (only-in laundry/tokenizer paragraph-make-tokenizer bind-runtime-todo-keywords))
 (for-syntax (rename-in (only-in laundry/heading parse parse-to-datum)
                        [parse parse-heading]
                        [parse-to-datum parse-heading-to-datum])
             (rename-in (only-in laundry/paragraph parse parse-to-datum)
                        [parse parse-paragraph]
                        [parse-to-datum parse-paragraph-to-datum]))
 (for-syntax racket/base
             racket/syntax
             racket/stxparam
             racket/pretty
             syntax/parse
             (only-in racket/string string-trim string-split)
             #;
             (only-in racket/list splitf-at)
             
             ))
(provide
 (rename-out [laundry-module-begin #%module-begin])
 (except-out (all-from-out racket/base) #%module-begin)
 (except-out (all-defined-out) laundry-module-begin))

; FIXME someone let me bind this stuff at runtime and then
; parameterize it later basically I want to be able to parse the first
; section of the file, bind the runtime values, and then parse the
; rest of the syntax, I probably have to do that with a syntax local
; variable or something like that though to make the macros cooperate
; rather than trying to pass it in
; XXX possibly via syntax-local-value or something? so that #+todo: statements expand the syntax value?
(define-for-syntax runtime-todo-keywords (make-parameter '("TODO" "DEFAULT"))) ; FIXME centralize?

(define-syntax (laundry-module-begin stx)
  (syntax-parse stx
    ([_ ast]
     #:do [
           #;
           (pretty-write `(laundry-module-begin: ,(syntax->datum #'ast)))]
     #'(#%module-begin
        (provide root)
        (define root ast)
        ; apparently the root binding can only be accessed inside here
        ; but not if the module is executed directly? not sure if this
        ; is a racket-mode issue or what?
        (require racket/pretty)
        #;
        (pretty-write root)
        (module+ main
          root)))))

#;
(define-syntax (org-file stx)
  (syntax-parse stx
    ([_ body ...]
     #'(define root (cons 'org-file body ...))
     #;
     #'(define root (quote (org-file body ...))))))

(define-syntax (define-node stx)
  (syntax-parse stx
    ([_ name]
     #:with elipsis (datum->syntax this-syntax '...)
     #'(define-syntax (name stx)
         (syntax-parse stx
           [(_ body elipsis)
            #'(cons 'name (list body elipsis))])))))

(define-syntax (define-nodes stx)
  (syntax-parse stx
    #:datum-literals (sa)
    [(_ (~alt name (sa sa-name)) ...)
     (if (attribute name)
         #'(begin (define-node name) ...)
         #'(begin
             (~@ ; FIXME pretty sure this is not working as desired
              (define-node sa-name)
              (define-rename-transformer-parameter sa-name (make-rename-transformer #'sa-name))) ...
             ))
     ]))

#; ; not needed because name is factored out unlike for define-node
(define-syntax (define-sa-node stx)
  (syntax-parse stx
    [(_ name)
     #:with elipsis (datum->syntax this-syntax '...)
     #:with elipsis-plus (datum->syntax this-syntax '...+)
     #'(define-syntax (name stx)
         (syntax-parse stx
           [(_) #'""] ; needed for cases like parl-indent
           [(_ body)
            (local-expand #'body 'expression #f)]
           [(_ body elipsis-plus)
            #:with appended
            (datum->syntax
             #'(body elipsis)
             (apply string-append
                    (let ([sigh (map (λ (e) (syntax->datum (local-expand e 'expression #f)))
                                     (syntax-e #'(body elipsis)))])
                      #;
                      (pretty-write (cons 'sighsighsigh: sigh))
                      sigh)))
            #'appended]))]))

(define-syntax (sa-node stx)
  (syntax-parse stx
    [(_) #'""] ; needed for cases like parl-indent
    [(_ body)
     (local-expand #'body 'expression #f)]
    [(_ body ...+)
     (datum->syntax
      #'(body ...)
      (apply string-append
             (map (λ (e)
                    ; FIXME we need to collect malformed properties here I think
                    (syntax->datum (local-expand e 'expression #f)))
                  (syntax->list #'(body ...)))))]))

(define-syntax (define-sa-nodes stx)
  (syntax-parse stx
    [(_ name ...)
     #'(begin (define-syntax name (make-rename-transformer #'sa-node)) ...)]))

(define-syntax (define-sa-alt-node stx)
  (syntax-parse stx
    ([_ name]
     #:with node-name
     (datum->syntax
      #'name
      (string->symbol
       (format "node-~a" (syntax-e #'name))))
     #:with sa-name
     (datum->syntax
      #'name
      (string->symbol
       (format "sa-~a" (syntax-e #'name))))
     #:with elipsis (datum->syntax this-syntax '...)
     #'(begin
         #; ; we don't actually need this because sa-node can be used directly as a syntax parameter
         (define-rename-transformer-parameter sa-name (make-rename-transformer #'sa-node))
         (define-syntax (node-name stx)
          (syntax-parse stx
            [(_ body elipsis)
             #'(cons 'name (list body elipsis))]))
         (define-rename-transformer-parameter name (make-rename-transformer #'node-name))
         ))))

(define-syntax (define-sa-alt-nodes stx)
  ; FIXME this macro is broken
  (syntax-parse stx
    [(_ name ...)
     #'(begin (define-sa-alt-node name) ...)]
    #;
    [(_ prefix name ...)
     #:with elipsis (datum->syntax this-syntax '...)
     #:with ((node-name sa-name) ...)
     (datum->syntax
      #'(name ...)
      (map
       (λ (n)
         (list
          (datum->syntax
           n
           (string->symbol
            (format "node-~a" (syntax-e n))))
          (datum->syntax
           n
           (string->symbol
            (format "~a-~a"
                    (syntax-e #'prefix) (syntax-e n))))))
       (syntax-e #'(name ...))))

     #'(begin
         (define-sa-alt-node name) ...
        )
     #;
     #'(begin
         (define-sa-node sa-name) ...
         (define-node node-name) ...
         (define-rename-transformer-parameter name (make-rename-transformer #'node-name)) ...
         #;
         (begin-for-syntax
           (define-syntax-parameter name
             (λ (stx) (error 'aaaaaaaaaaaaaaaaaaaa))
             #;
             (make-rename-transformer #'name))) ;...
         #;
         (define-node name) ;...
         #;
         (begin-for-syntax
           (define-syntax-parameter name
             (λ (stx)
               ; XXX should always be aligned with define-node
               (syntax-parse stx
                 [(_ body-inner elipsis)
                  ;#:do [(println #'(body-inner elipsis))]
                  #'(cons 'name (list body-inner elipsis))])))
           ; ...
           ))]))

(define-syntax (define-sa-keep-node stx)
  (syntax-parse stx
    ([_ name]
     #:with elipsis (datum->syntax this-syntax '...)
     #'(define-syntax (name stx)
         (syntax-parse stx
           [(_ body elipsis)
            #'(list 'name (string-append body elipsis))])))))

(define-syntax (define-sa-keep-nodes stx)
  (syntax-parse stx
    [(_ name ...)
     #'(begin (define-sa-keep-node name) ...)]))

(define-sa-keep-nodes
  #; ; FIXME h-title must use the paragraph parser internally
  h-title
  plain-list-line-tail
  table-cell)

(define-sa-nodes ; XXX things in this list should probably be spliced out
  not-newline
  not-whitespace
  ns-nwt-less-negated
  not-pl-start-whitespace1
  stars
  alphas
  alpha-n
  alphas-unmixed ; only the parser needs to know that they are unmixed

  headline-content ; this is sa because we have to remerge the whole headline for the 2nd pass parser

  ; put the headline titles here for now until we can figure out what
  ; to do about the markup for titles
  ; all spliced out now
  ;h-title-r-p-c
  ;h-title-p-c
  ;h-title-c
  ;;h-title

  parl-start
  parl-indent
  parl-wsnn
  parl-prp-bt
  parl-ncn-bt-l-d
  not-prp-newline1
  parl-ws-bt-l-s
  parl-ws-bt
  parl-pw-bt
  parl-se-wsnn
  parl-sigh
  parl-tokens-with-newline
  malformed-nl ; FIXME -> syntax warn
  detached-drawer

  stuff
  wordhyus
  word-char-n
  blank-line
  not-alpha-newline1
  not-colon-newline
  not-colon-newline1
  not-colon-whitespace
  not-colon-whitespace1

  not-colon-lsb-whitespace
  not-colon-lsb-whitespace1
  not-colon-rsb-newline
  not-colon-rsb-newline1
  not-rsb-newline

  not-plus-whitespace1
  not-whitespace-l-d
  not-whitespace1
  not-asterisk-whitespace1

  big-tokes-less-d-s
  big-tokes-less-d-s-p
  big-tokes-less-d-s-p-cnt
  big-tokes-less-d-s-p-cnt-blk
  big-tokes-less-d-s-blk
  bt-chars-no-title-start
  bt-chars-plan
  bt-chars
  bt-colon
  bt-hash
  )

(define-sa-nodes ; malformed case
  ; FIXME I'm not entirely sure what to do about the malformed bits
  ; we should be able to stash that information in the syntax object
  ; when we roll these up to be strings again for reparsing as a paragraph

  sa-malformed
  #|
  sa-blk-src-begin
  sa-blk-src-line-contents
  sa-end-drawer
  |#

  detached-block-node
  blk-greater-malformed
  det-blk-ex-begin
  det-blk-ex-end
  det-blk-src-begin
  det-blk-src-end
  ; blk-src-begin sa variant
  planning-dissociated
  ak-key-no-colon
  babel-call-no-colon
  )

(define-sa-alt-nodes
  blk-src-begin
  blk-src-line-contents

  language
  blk-src-line-rest-alt
  switches-sane
  switch-sane
  format-string
  format-string-contents
  blk-src-parameters
  blk-src-args-broken
  blk-src-end

  end-drawer
  )
#;
(begin-for-syntax
  (define-rename-transformer-parameter blk-src-begin (make-rename-transformer #'node-blk-src-begin)))
(define-nodes
  org-file
  org-node
  org-node-dyn
  comment-element
  comment-line

  headline-node
  ;headline
  ;heading ; FIXME we need to deal with the number of stars here
  heading-content
  todo-keyword
  h-priority
  h-comment
  ;h-tags
  archive

  plain-list-line
  descriptive-list-line
  ordered-list-line
  pl-indent
  bullet-counter
  bullet-plain
  drawer
  drawer-name
  #;
  end-drawer ; end is tricky because we do want to warn on it
  table
  table-row
  table-row-rule
  table-node
  hyperlink ; FIXME needs to be an sa alt probably

  bof
  digits
  nlpws ; this can't be saed because it contains an indent that is needed in some cases?

  property-drawer
  pdrawer-unparsed ; FIXME naming ??

  keyword
  keyword-key
  keyword-value
  keyword-key-sigh
  keyword-value-sigh
  keyword-options

  ; TODO -> new keyword approach
  keyword-node
  kw-key-options
  kw-key
  kw-value

  affiliated-keyword
  un-affiliated-keyword
  name
  header
  plot
  ak-key
  ak-value
  ak-key-attr
  attr-backend
  ak-opt
  ak-key-opt
  ak-key-name-opt

  babel-call

  footnote-definition
  footnote-definition-line
  fn-label
  fn-def

  blk-src

  blk-src-contents
  blk-src-whole
  blk-src-malformed

  block-begin-line
  block-end-line

  blk-ex-begin
  blk-ex-end

  blk-greater-begin
  blk-greater-type
  block-type-name
  bg-type-special
  block-type-rest
  blk-greater-end
  bg-end-special

  blk-dyn
  blk-dyn-begin
  blk-line-contents
  blk-dyn-contents
  blk-dyn-end

  macro

  bold
  italic
  underline
  strike-through
  verbatim
  code

  #|
  bold-italic
  bold-underline
  bold-strike-through
  italic-underline
  italic-strike-through
  underline-strike-through

  bold-italic-strike-through
  bold-italic-underline
  bold-underline-strike-through
  italic-underline-strike-through

  bold-italic-underline-strike-through
  |#
  )

(define-nodes ; test parsers
  --test--switches-sane
  --test--heading-rest
  )

(define-syntax (newline stx) (syntax/loc stx "\n"))
(define-syntax (space stx) (syntax/loc stx " "))
(define-syntax (tab stx) (syntax/loc stx "\t"))

(define-syntax (blank stx)
  (syntax-parse stx
    ([_ thing:str]
     #'thing)))

; heading
(begin-for-syntax
  (define-syntax-parameter newline
    ; XXX we're going to have to switch this so that the org-node newline case can be handled?
    (lambda (stx) 'newline))
  #; ; par-nl needs to be syntax so expansion can happen at compile time
  (define (par-nl . rest)
    "\n")
  (define-syntax (par-nl stx)
    (syntax-parse stx
      [(_ ignore)
       #'"\n"]))
  (define-syntax-parameter end
    (lambda (stx) 'end))
  (define (par-end end)
    end)

  (define-syntax-parameter stars
    (lambda (stx) 'stars))

  )

#;
(define-syntax (heading-stars stx)
  (syntax-parse stx
    [(_ stars)
     (datum->syntax
      #'stars
      (string-length
       (string-trim
        (syntax->datum
         #'stars))))]))

(define-syntax (headline stx)
  (syntax-parse stx
    [(_ body ...)
     ;#:do [(define int-ctx (syntax-local-make-definition-context))]
     (datum->syntax
      #'(body ...)
      (let ([heading-input-raw
             (apply string-append
                    (map (λ (e)
                           (syntax->datum
                            (local-expand e 'expression #f)))
                         (syntax->list #'(body ...))))])
        (do-headline heading-input-raw)))]))

(define-syntax (heading stx)
  (syntax-parse stx
    #:literals (stars)
    [(_ (stars sstring) pct ... tags)
     #:with depth (datum->syntax
                   #'sstring
                   (string-length
                    (string-trim
                     (syntax->datum
                      #'sstring))))
     #:with (other ...)
     ; TODO priority comment title
     #'(pct ...)
     #:with tags-list (let ([t (syntax->datum #'tags)])
                        (if t
                            (local-expand #'tags 'expression #f)
                            #''()))
     #'(cons 'heading (list depth other ... tags-list))])
  #;
  (syntax-parse stx
    ([_ stars content tags]
     (local-expand )
     #'(heading depth content tag-list)
     )))

(define-for-syntax (process-tag-string tag-string)
  (datum->syntax
   tag-string
   (cons 'list
         (string-split
          (string-trim
           (syntax->datum tag-string)
           #px"\\s+|:") ":"))))

(define-syntax (h-tags stx)
  (syntax-parse stx
    [(_ tag-string:str)
     (process-tag-string #'tag-string)]))

(define-syntax (h-title stx)
  (syntax-parse stx
    [(_ body ...)
     #:do [(define title-input
             (apply string-append
                    (map (λ (e)
                           (syntax->datum
                            (local-expand e 'expression #f)))
                         (syntax->list #'(body ...)))))]
     #:with (expanded ...) (local-expand ; FIXME I'm guessing that this loses the syntax provenance
                            (datum->syntax
                             #'(body ...)
                             (do-paragraph title-input))
                            'expression #f)
     #'(list 'h-title expanded ...)]))

(define-syntax h-t-rpc-t (make-rename-transformer #'h-title))
(define-syntax h-t-pc-t (make-rename-transformer #'h-title))
(define-syntax h-t-c-t (make-rename-transformer #'h-title))

(define-for-syntax (with-newlines str)
  (let ([prepended (if (eq? (string-ref str 0) #\newline)
                       str
                       (string-append "\n" str))] )
    (if (eq? (string-ref str (sub1 (string-length str))) #\newline)
        prepended
        (string-append prepended "\n"))))

(define-for-syntax (do-headline str)
  (let* ([heading-make-tokenizer (bind-runtime-todo-keywords (runtime-todo-keywords))]
         [heading-input (with-newlines str)]
         #;
         [__ (displayln (format "heading-input: ~s" heading-input))]
         [out (parse-heading-to-datum
               (heading-make-tokenizer
                (open-input-string heading-input)))])
    #;
    (pretty-write (list 'do-heading-out: out))
    out))

; paragraph

(define-syntax (markup-terminal stx)
  (syntax-parse stx
    [(_ (type text:str))
     #:with trimmed
     (let* ([dat-text (syntax-e #'text)]
            [lt (string-length dat-text)])
       (datum->syntax #'text (substring dat-text 1 (sub1 lt))))
     #'(type trimmed)
     ])
  )
(define-syntax (markup-rec stx)
  ; FIXME we may need a markup lexer which has slighly different
  ; properties for inner markup, specifically wrt starting with a
  ; marker
  (syntax-parse stx
    ((_ (type text:str))
     #:do [(define dat-text (syntax-e #'text))
           (define dat-type (syntax->datum #'type))
           (define leading-len
             (caar
              (regexp-match-positions
               (case dat-type
                 ((bold) #rx"\\*")
                 ((italic) #rx"/")
                 ((underline) #rx"_")
                 ((strike-through) #rx"\\+")
                 #;
                 ((verbatim) #rx"=")
                 #;
                 ((code) #rx"~"))
               dat-text)))
           #;
           (println dat-text)]
     ;#:with leading (datum->syntax #'text (substring dat-text 0 leading-len))
     ; #:with text-clean (datum->syntax #'text (substring dat-text 2 (sub1 (string-length dat-text))))
     #:with (more-par ...)
     (datum->syntax #'text
                    (cdr (do-paragraph
                          (substring dat-text
                                     (add1 leading-len)
                                     (sub1 (string-length dat-text)))))) ; FIXME base case

     ;#:with (expanded ...) (map (λ (e) (local-expand e 'expression #f)) (syntax-e #'more-par))
     ;#:do [(pretty-write (datum->syntax))]
     (let ([out #'(type more-par ...)])
       #;
       (pretty-write (cons 'mumumumu: (syntax->datum out)))
       out
       )
     #;
     #'(unquote-splicing (list leading (type text-clean))))))

(begin-for-syntax
  (define-namespace-anchor anc-mal)
  (define ns-mal (namespace-anchor->namespace anc-mal)))
(define-syntax (malformed stx)
  (syntax-parse stx
    [(_ body ...)
     (define-values (transparent opaque)
       (syntax-local-expand-expression
        ; we can't bind the mrt part as a variable?
        #'(syntax-parameterize ([blk-src-begin (make-rename-transformer #'sa-node)]
                                [blk-src-line-contents (make-rename-transformer #'sa-node)]
                                [language (make-rename-transformer #'sa-node)]
                                [blk-src-line-rest-alt (make-rename-transformer #'sa-node)]
                                [switches-sane (make-rename-transformer #'sa-node)]
                                [switch-sane (make-rename-transformer #'sa-node)]
                                [format-string (make-rename-transformer #'sa-node)]
                                [format-string-contents (make-rename-transformer #'sa-node)]
                                [blk-src-parameters (make-rename-transformer #'sa-node)]
                                [blk-src-args-broken (make-rename-transformer #'sa-node)]
                                [blk-src-end (make-rename-transformer #'sa-node)]
                                [end-drawer (make-rename-transformer #'sa-node)]
                                )
            (sa-malformed body ...))))
     (syntax-property
      (datum->syntax
       #'(body ...)
       (parameterize ([current-namespace ns-mal])
         (eval transparent)))
      'malformed this-syntax)]))

(module+ test-mal
  ;#; ; somehow this works at the top level but not in module scope !?
  ; WAIT !? somehow this is working now !??!?
  ; argh, it works here but not in a requiring module !?
  (paragraph-node (malformed (blk-src-begin "oops")))
  )

(define-syntax (paragraph stx)
  (syntax-parse stx
    [(_ body ...)
     #:with (expanded ...)
     (map (λ (e)
            (when (syntax-property e 'malformed)
              ; TODO syntax warn probably? also this doesn't seem to work?
              ; maybe the malformed annotation is getting lost during an sa?
              (println (format "Found malformed structure! ~s" e)))
            (local-expand e 'expression #f))
          (syntax->list #'(body ...)))
     ;#:do [(pretty-write (cons 'paragraph: (syntax->datum #'(expanded ...))))]
     #'(expanded ...)]))

(define-syntax (paragraph-node stx)
  (syntax-parse stx
    [(_ body ...)
     #:with (expanded ...)
     ; FIXME for malformed patterns we will need to flag that they are
     ; malformed, record their locations, and add them to syntax warn
     ; or something like that
     (local-expand
      (datum->syntax
       #'(body ...)
       (do-paragraph
        (apply string-append
               (map (λ (e)
                      (syntax->datum
                       (local-expand e 'expression #f)))
                    (syntax-e #'(body ...))))))
      'expression
      #f)
     (let ([out #'(list 'paragraph expanded ...)])
       #;
       (pretty-write (cons 'p1p1p1: (syntax->datum out)))
       out)
     ]))

(define-for-syntax (do-paragraph str)
  ; FIXME this is gonna be a bit of work
  ; we use -to-datum here because there are two different paragraphs
  ; FIXME this is a bit busted maybe?
  (let ([out (parse-paragraph-to-datum
              (paragraph-make-tokenizer
               (open-input-string str)))])
    #;
    (pretty-write (cons 'do-paragraph: out))
    out))

(define (merge-thing do-fun l)
  (if (null? l)
      l
      (let ([f (car l)]
            ; see if the paragraph parser is causing the slowdown
            #;
            [do-fun identity])
        (if (string? f)
            (let-values ([(a b) (splitf-at l string?)])
              (cons (do-fun (apply string-append a)) (merge-thing do-fun b)))
            (let-values ([(a b) (splitf-at l (compose not string?))])
              (append a (merge-thing do-fun b)))))))

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

;; keywords

(define-syntax (keyword-whole-line stx)
  ; process keyword line
  (syntax-parse stx
    [(_ line:str)
     ; TODO
     #'(list 'keyword-line line)]))

(define-syntax (kw-prefix stx)
  ; XXX temporary implementation detail to compensate for e.g. #+title being a token
  (syntax-parse stx
    [(_ with-prefix:str)
     #:with without-prefix (datum->syntax #'with-prefix (substring (syntax-e #'with-prefix) 2))
     #'(keyword-key without-prefix)]))

(define-syntax (todo-spec-line stx)
  ; XXX TODO this is where we have to bind the runtime todo keywords
  (syntax-parse stx
    [(_ line:str)
     #'(cons 'todo-spec-line line) ; XXX TODO
     ]))
