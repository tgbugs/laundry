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
 (only-in laundry/grammar/org make-rule-parser)
 ;(only-in laundry/tokenizer ) ; FIXME -> syntax time
 (for-syntax
  (only-in laundry/tokenizer
           table-make-tokenizer
           paragraph-make-tokenizer
           bind-runtime-todo-keywords)
  (rename-in (only-in laundry/grammar/heading parse parse-to-datum)
             [parse parse-heading]
             [parse-to-datum parse-heading-to-datum])
  (rename-in (only-in laundry/grammar/paragraph parse parse-to-datum)
             [parse parse-paragraph]
             [parse-to-datum parse-paragraph-to-datum])
  (rename-in (only-in laundry/grammar/table parse parse-to-datum)
             [parse parse-table]
             [parse-to-datum parse-table-to-datum])
  racket/base
  racket/syntax
  racket/stxparam
  racket/pretty
  syntax/parse
  (only-in racket/string string-trim string-split string-join string-contains?)
  (only-in racket/list last remove-duplicates)))

(define-for-syntax debug (make-parameter #f))

(provide
 (rename-out [laundry-module-begin #%module-begin])
 (except-out (all-from-out racket/base) #%module-begin)
 (except-out (all-defined-out) laundry-module-begin))

; TODO I think this technically already IS module local because when each module is expanded
; as a module written in the laundry/expander language this parameter should be redefined!
; however, I'm not 100% sure on this
; FIXME check on what the behavior is for changing the default todo cycle from #+todo: TODO DONE
; I think the right way to do this is to have a check when we hit the first heading? maybe?
; XXX the only restriction that this implementation has on the relative position of
; #+todo: lines and use of todo keywords is that the #+todo: line must always occure
; before use IN THE file NOT JUST in time, i.e. you cannot just refresh the setup
; of a file, the setup is refreshed automatically if certain modifications are moade
; currently at the expense of performance, but hopefully not necessarily so
; FIXME rather obviously in hindsight, these are actually expand-time-todo-keywords
(define-for-syntax runtime-todo-keywords (make-parameter '("TODO" "DONE")))
(define-for-syntax runtime-todo-keywords-changed (make-parameter #f))

(module merge-helper racket/base
  (require (only-in racket/list last))

  (provide merge-strings merge-syntaxes)

  (define (merge-strings lst)
    "fold a list merging strings as we go"
    ; I have no idea if there is a faster way to do this, but it certainly feels elegant
    #;
    (log-error "merge-strings: ~a" lst)
    (foldl
     (λ (new old)
       (cond
         [(null? old)
          (cons new old)]
         [(and (string? new) (string? (car old)))
          (cons (string-append new (car old)) (cdr old))]
         [else (cons new old)]))
     '() ; old
     (reverse lst)))

  (define (merge-syntaxes lst context) ; FIXME broken because some incoming syntaxes have #f locations
    "fold a list merging syntax as we go"
    ; I have no idea if there is a faster way to do this, but it certainly feels elegant
    (log-error "merge-syntaxes: ~a" lst)
    (if (= (length lst) 1)
        lst
        (cdr ; we pad with a dead pair to avoid special cases
         (foldl
          (λ (new-pair old)
            (if (null? old)
                (cons new-pair old)
                (let ([new-str (car new-pair)]
                      [old-str (caar old)])
                  (cond
                    [(and new-str old-str)
                     (cons
                      (cons ; car
                       (string-append new-str old-str) ; caar
                       (cons (cadr new-pair) (cdar old))) ; cdar
                      (cdr old))]
                    [old-str ; we have hit a non-string element, clean up the old car
                     ; everything is reversed in here so first is last and last is first
                     ; which means that the start position for the syntax is contained in the
                     ; last element of the old-syntaxes list
                     (let*-values ([(old-syntaxes) (cdar old)]
                                   [() (when #f
                                         (println (list 'old-syntaxes:
                                                        (map (λ (s) (list s
                                                                          (syntax-line s)
                                                                          (syntax-source s)
                                                                          (syntax-span s)))
                                                             old-syntaxes))) (values))]
                                   [(syntax-end syntax-start) (values (car old-syntaxes) (last old-syntaxes))]
                                   [(start-pos) (syntax-position syntax-start)]
                                   [(syntax-loc)
                                    (if (eq? syntax-start syntax-end)
                                        syntax-start
                                        (list
                                         (syntax-source syntax-start)
                                         (syntax-line syntax-start)
                                         (syntax-column syntax-start)
                                         start-pos
                                         (apply + (map syntax-span old-syntaxes))
                                         #;
                                         (+ (- (syntax-position syntax-end) start-pos)
                                            (syntax-span syntax-end))))])
                       (println (list 'syntax-loc syntax-loc syntax-start syntax-end))
                       (cons
                        new-pair
                        (cons (datum->syntax context old-str syntax-loc)
                              (cdr old))))]
                    ; toss the old caar retaining just the syntax
                    [else (cons
                           new-pair
                           (cons
                            (cdar old)
                            (cdr old)))]))))
          '() ; old
          (let ([string-list
                 (map (λ (stx)
                        (let ([dat (syntax-e stx)])
                          (cons (and (string? dat) dat) (list stx))))
                      lst)])
            (log-error "merge-syntax-string-list: ~a" string-list)
            (reverse (cons (cons #f #f) string-list)))))))

  (module+ test-merge
    (require rackunit)
    (require (only-in "tokenizer.rkt" paragraph-make-tokenizer))
    (require (rename-in "grammar/paragraph.rkt" [parse parse-paragraph]))
    (check-equal?
     (merge-strings
      '("a" "b" c "d" "e" f g "h"))
     '("ab" c "de" f g "h"))
    ))

(require (for-syntax 'merge-helper))

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
        (module+ main
          root)))))

(define-syntax (define-node stx)
  (syntax-parse stx
    ([_ name]
     #:with elipsis (datum->syntax this-syntax '...)
     #'(define-syntax (name stx)
         (syntax-parse stx
           [(_ body elipsis)
            #:do [(when (debug) (println (list 'define-node #'name (syntax->datum #'(body elipsis)))))]
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

(define-syntax (sa-node stx)
  (syntax-parse stx
    [(_) #'""] ; needed for cases like parl-indent
    [(_ body)
     (local-expand #'body 'expression #f)]
    [(_ body ...+)
     #:do [(when (debug) (println (list 'sa-node (syntax->datum #'(body ...)))))]
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
     #'(begin (define-sa-alt-node name) ...)]))

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

(define-sa-nodes ; XXX things in this list should probably be spliced out
  stars

  paragraph-line

  detached-drawer

  stuff
  )

(define-sa-nodes ; malformed case
  ; FIXME I'm not entirely sure what to do about the malformed bits
  ; we should be able to stash that information in the syntax object
  ; when we roll these up to be strings again for reparsing as a paragraph

  sa-malformed

  ;detached-block-node
  detached-block ; FIXME not sure if we need this
  ; blk-src-begin sa variant
  planning-dissociated
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

  footnote-inline-malformed
  footnote-inline-malformed-eof

  keyword-malformed
  )

(define-nodes
  org-file
  org-node
  org-node-dyn
  empty-line
  double-blank-line
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
  tags ; XXX internal should probably be a struct field
  archive

  planning-malformed

  plain-list-element
  plain-list-line ; TODO when we do the ast transformer pass it turns out that only the #+begin_src line needs to be indented (what a relief!)
  descriptive-list-line
  ordered-list-line
  pl-indent
  bullet-counter
  bullet-plain

  dynamic-block

  drawer
  drawer-name
  #;
  end-drawer ; end is tricky because we do want to warn on it
  table
  table-row
  table-row-rule

  property-drawer
  pdrawer-unparsed ; FIXME naming ??

  ; TODO -> new keyword approach
  keyword-node
  ;kw-key-options
  ;kw-key
  ;kw-value

  ;keyword
  ;keyword-key
  ;keyword-value
  ;keyword-key-sigh
  ;keyword-value-sigh
  ;keyword-options



  greater-block

  ;block-begin-line
  ;block-end-line

  ;; objects

  latex-entity-fragment
  latex-fragment
  latex-fragment-n
  latex-fragment-parens
  export-snippet
  citation

  inline-call
  inline-call-malformed
  inline-src-block

  hyperlink ; FIXME needs to be an sa alt probably
  link-angle
  link-regular

  macro ; TODO determine the phase at which macros expand

  noweb-target
  radio-target
  stats-cookie
  superscript
  subscript
  script-paren

  timestamp ; TODO deeper parsing

  footnote-reference ; FIXME likely needs a separate implementation
  #;
  footnote-anchor
  footnote-anchor-inline
  #;
  footnote-inline

  footnote-definition
  footnote-definition-inline
  #;
  fn-label
  #;
  fn-def

  bold
  italic
  underline
  strike-through
  verbatim
  code
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
     #:do [(when (debug) (println (list 'headline (syntax->datum #'(body ...)))))]
     (datum->syntax
      #'(body ...)
      (let ([heading-input-raw
             (apply string-append
                    (map (λ (e)
                           (syntax->datum
                            (local-expand e 'expression #f)))
                         (syntax->list #'(body ...))))])
        (do-heading heading-input-raw this-syntax)))]))

(define-syntax (heading stx)
  (syntax-parse stx
    #:literals (stars)
    [(_ (stars sstring) pct ... tags-string)
     #:with depth (datum->syntax
                   #'sstring
                   (string-length
                    (string-trim
                     (syntax->datum
                      #'sstring))))
     #:with (other ...)
     ; TODO priority comment title
     #'(pct ...)
     #:with tags-list (let ([t (syntax->datum #'tags-string)])
                        (if t
                            (local-expand #'tags-string 'expression #f)
                            #''(tags)))
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
   (cons 'tags ; TODO struct probably
          (let ([trimmed
                 (string-trim
                  (syntax->datum tag-string)
                  ; don't use \\s+ and #t you can use \\s+ and #f or \\s and #t
                  #px"\\s|:" #:repeat? #t)])
            (if (string-contains? trimmed ":")
                (remove-duplicates
                 (string-split
                  trimmed
                  ":"
                  #:trim? #f))
                (list trimmed))))))

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
                             (do-paragraph title-input this-syntax))
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

(define-for-syntax heading-make-tokenizer (make-parameter #f))

(define-for-syntax (do-heading str [original-syntax #f])
  (when (debug)
    (println (list "do-heading runtime-todo-keywords:" (runtime-todo-keywords))))
  (with-handlers ([exn:fail? (λ (e) ((error-display-handler) (exn-message e) e)
                               (raise-syntax-error #f "happened in" original-syntax))])
    (let* ([heading-make-tokenizer
            (let ([hmt (heading-make-tokenizer)])
              (if (or (not hmt) (runtime-todo-keywords-changed))
                  ; avoid waisting cycles making a new heading tokenizer
                  ; if todo keywords have not changed
                  (let ([hmt-new (bind-runtime-todo-keywords (runtime-todo-keywords))])
                    (heading-make-tokenizer hmt-new)
                    (runtime-todo-keywords-changed #f)
                    hmt-new)
                  hmt))]
           [heading-input (with-newlines str)]
           #;
           [__ (displayln (format "heading-input: ~s" heading-input))]
           [out (parse-heading-to-datum
                 (heading-make-tokenizer
                  (open-input-string heading-input)))]
           [merged (merge-strings out)]
           )
      (when (debug)
        (pretty-write (list 'do-heading-out/merged: out merged)))
      merged)))

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
                 #; ; handled via terminal
                 ((verbatim) #rx"=")
                 #; ; handled via terminal
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
                                     (sub1 (string-length dat-text)))
                          #'text))) ; FIXME base case

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
     #:do [(when (debug) (println (list 'malformed (syntax->datum #'(body ...)))))]
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
                                [footnote-inline-malformed (make-rename-transformer #'sa-node)]
                                [footnote-inline-malformed-eof (make-rename-transformer #'sa-node)]
                                [keyword-malformed (make-rename-transformer #'sa-node)]
                                )
            (sa-malformed body ...))))
     (syntax-property
      (datum->syntax
       #'(body ...)
       (parameterize ([current-namespace ns-mal])
         (eval transparent)))
      'malformed this-syntax)]))

; FIXME this should all be subsumed and malformed should be for whole lines
(define-syntax malformed-nl (make-rename-transformer #'malformed))

(module+ test-mal
  ;#; ; somehow this works at the top level but not in module scope !?
  ; WAIT !? somehow this is working now !??!?
  ; argh, it works here but not in a requiring module !?
  (paragraph-node (malformed (blk-src-begin "oops")))
  (paragraph-node "a\nb\nc\nd\ne")
  )

(define-syntax (footnote-anchor stx)
  (syntax-parse stx
    [(_ start:str)
     #:with label (let* ([str (syntax-e #'start)]
                         [ss (string-split (substring str 0 (sub1 (string-length str))) ":")]
                         [label-raw (cadr ss)]
                         ; FIXME label symbol will cause error on reparse so we need another name
                         [label (if (> (string-length label-raw) 0)
                                    label-raw
                                    (list 'quote (gensym))
                                    )])
                    (datum->syntax #'start label))
     #'(list 'footnote-anchor label)]))

(define-syntax (footnote-inline stx)
  (syntax-parse stx
    [(_ start:str body ...)
     ; TODO parse the label out FIXME during expansion, so long as we
     ; can put the definition back inline it should be ok to pull the
     ; definitions out to the to level and even make it possible to
     ; interconvernt compatible definitions from end of section to
     ; inline and vice versa ... I think we just use gensym on the
     ; unlabeled ones or something and keep track so we know that they
     ; were originally inline?
     ; (footnote-reference (footnote-anchor) (footnote-definition-inline body ...))
     #:with label (let* ([ss (string-split (syntax-e #'start) ":")]
                         [label-raw (cadr ss)]
                         ; FIXME label symbol will cause error on reparse so we need another name
                         [label (if (> (string-length label-raw) 0)
                                    label-raw
                                    (list 'quote (gensym))
                                    )])
                    #;
                    (println (list "inline start:" #'start "inline ss:" ss))
                    (datum->syntax #'start label))
     #:do [
           #;
           (println (list "should be doing footnote-inline:" (syntax->datum #'(body ...))))
           #;
           (println (list "inline label:" #'label))
           ]

     ; FIXME need separate naming for the internal transformed version of these
     ; FIXME in theory you could dispatch on string vs symbol for labeled vs unlabeled, but it doesn't quite
     ; work because we also want to be able to distinguish between inline vs not-inline
     #'(list 'footnote-inline (footnote-anchor-inline label) (footnote-definition-inline label body ...))
     #;
     #'(list 'footnote-inline #:ref label body ...)]))

(module+ test-footnote-inline
  (footnote-inline "[fn:y:" (paragraph-inline "N."))
  (footnote-inline-simple "[fn:y:N.]")

  (footnote-inline "[fn::" (paragraph-inline "N."))
  (footnote-inline-simple "[fn::N.]")

  (paragraph-node "B [fn::I[fn::N.].] A.")

  (paragraph-node "B [fn:x:I[fn:y:N.].] A.")

  (paragraph-inline "I" (footnote-reference (footnote-inline-simple "[fn::N.]")) ".")

  (footnote-reference (footnote-inline-simple "[fn::N.]"))

  (footnote-inline-simple "[fn::N.]")

  (paragraph-inline "I" (footnote-reference (footnote-inline-simple "[fn::N.]")) ".")

  (paragraph-inline "a" "b" "c" "d" "e" "f" "g")

  )

(define-syntax (footnote-inline-simple stx)
  (syntax-parse stx
    [(_ body:str)
     #:with (label content)
     (let* ([str (syntax-e #'body)]
            [ss (substring str 0 (sub1 (string-length str)))]
            ; FIXME TODO #:trim? needs careful consideration with respect to leading whitespace and roundtripping
            [sp (string-split ss ":" #:trim? #f)]
            [label (cadr sp)]
            [body (cddr sp)])
       (list (datum->syntax #'body (string-append (car sp) ":" label ":"))
             (datum->syntax #'body (string-join body ":"))))
     (let ([out #'(footnote-inline label (paragraph-inline content))])
       #;
       (println (list "fis out:" out))
       out
       )]))

(define-syntax sup-p (make-rename-transformer #'script-paren))
(define-syntax sub-p (make-rename-transformer #'script-paren))

(define-for-syntax (paragraph-expand-body e)
  (let ([out (local-expand e 'expression #f)])
    (let ([dat (syntax-e out)])
      ; FIXME this loses the info on the syntax
      (if (string? dat) dat out))))

(define-syntax (paragraph stx)
  (syntax-parse stx
    [(_ body ...)
     #:with (expanded ...)
     (map (λ (s) (if (string? s)
                     (datum->syntax this-syntax s) ; FIXME URG PAIN
                     s))
          (merge-strings ; this is the correct place to do the merge
           (map (λ (e)
                  (when (syntax-property e 'malformed)
                    ; TODO syntax warn probably? also this doesn't seem to work?
                    ; maybe the malformed annotation is getting lost during an sa?
                    (println (format "Found malformed structure! ~s" e)))
                  (paragraph-expand-body e))
                (syntax->list #'(body ...)))))
     #:do [(when (debug)
             (pretty-write (cons 'paragraph: (syntax->datum #'(expanded ...)))))]
     #'(expanded ...)]))

(define-syntax (paragraph-inline stx)
  (syntax-parse stx
    [(_ body ...)
     #:with (expanded ...)
     (map (λ (s) (if (string? s)
                     (datum->syntax this-syntax s) ; FIXME URG PAIN
                     s))
          (merge-strings ; this is the correct place to do the merge

           (map (λ (e)
                  (when (syntax-property e 'malformed)
                    ; TODO syntax warn probably? also this doesn't seem to work?
                    ; maybe the malformed annotation is getting lost during an sa?
                    (println (format "Found malformed structure! ~s" e)))
                  (paragraph-expand-body e))
                (syntax->list #'(body ...)))))
     #'(list 'paragraph expanded ...)]))

(module+ test-paragraph-node

  ; FIXME some of these forms actually never make it through
  ; to the paragraph parser becuase the double newline
  ; is removed before we ever arrive
  (paragraph-node "[fn::\n\n")
  (paragraph-node "\n[fn::\n\n\n")

  (paragraph-node "[fn::")

  )

(define-syntax (paragraph-node stx)
  (syntax-parse stx
    [(_ body ...)
     #:do [(when (debug) (pretty-print (list "paragraph-node body:" (syntax->datum #'(body ...)))))
           (when (debug) (pretty-print (list "paragraph-node body:" (syntax->datum #'(body ...)))))]
     #:with (expanded ...)
     ; FIXME for malformed patterns we will need to flag that they are
     ; malformed, record their locations, and add them to syntax warn
     ; or something like that
     (let* ([expanded-raw
            (local-expand
             (datum->syntax
              #'(body ...)
              (do-paragraph
               ; FIXME it would be way, way, more efficient to chain in the
               ; tokenizer by peeking to find the end and then using a
               ; limited input port rather than passing all this garbage into
               ; the abstract syntax tree ? no, not quite because the grammar
               ; also also nested ? or no ? ... HRM regardless, it would be
               ; nice to be able to use the underlying port instead of the
               ; nonsense I do here ... HRM we can't do it in the lexer itself
               ; but maybe we could do it in next-token ?
               (let ([wat (map (λ (e)
                             (syntax->datum
                              (local-expand e 'expression #f)))
                               (syntax-e #'(body ...)))])
                 (when (debug)
                   (println (format "paragraph-node wat: ~a" wat)))
                 (apply string-append wat))
               this-syntax))
             'expression
             #f)])
       expanded-raw)

     (let ([out #'(list 'paragraph expanded ...)])
       (when (debug)
         (pretty-write (cons 'p1p1p1: (syntax->datum out))))
       out)]))

(define-for-syntax (do-paragraph str [original-syntax #f])
  ; FIXME this is gonna be a bit of work
  ; we use -to-datum here because there are two different paragraphs
  ; FIXME this is a bit busted maybe? yeah def busted, parse-paragraph should work but doesn't

  ; FIXME do we use hander-case here or something to deal with
  ; malformed?  I don't think we do because the malformed cases are
  ; things we should be able to handle the only issue is dealing with
  ; eol induced ambiguity
  #;
  (println (list "do-paragraph in:" str))
  (with-handlers ([exn:fail? (λ (e) ((error-display-handler) (exn-message e) e)
                               (raise-syntax-error #f "happened in" original-syntax))])
    (let* ([out-helper
            (parse-paragraph-to-datum
             (paragraph-make-tokenizer
              (let ([port (open-input-string (string-append "\n" str "\n"))])
                (port-count-lines! port)
                ; FIXME get a real port kids!
                port)))]
           [out (cons (car out-helper)
                      ; FIXME needs a bit of work if there are cases where the newline gets eaten
                      ; FIXME perf
                      ; FIXME do merge strings in here for maximum efficiency, avoids 2 reverses
                      (reverse (cdr (reverse (cddr out-helper)))))]
           [merged (merge-strings out)] ; FIXME not 100% sure this is actually the right place to do this since we have to do it again
           )
      #;
      (pretty-write (list 'do-paragraph: out-helper out merged (when original-syntax (syntax-position original-syntax) original-syntax)))
      merged)))

#; ; a bad implementation
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
     #;
     (println (list 'keyword-whole-line #'line))
     ; TODO
     (let ([line-str (string-trim (syntax-e #'line))]) ; TODO refactor to share with colorer?
       ; FIXME this is a horrible quickly hacked implementation
       ; keyword lines need to be parsed, and I still don't have a
       ; complete grasp of what the regularized
       ; #+key[options]:rest-no-colon-no-rsb looks like, especially
       ; with regard to nesting brackets
       (cond
         [(regexp-match #rx"^#\\+(todo|TODO):" line-str) ; FIXME sigh case sensitivity
          ; see the docstring for org-todo-keywords, these are NOT simple
          (let* ([groups (regexp-split #px"\\s+" (string-trim (substring line-str 7)))]
                 [todo-keywords (remove "|" groups)]
                 ; NOTE we DO handle the split into todos and dones here because
                 ; we are already looking at it, even though the only thing we are
                 ; going to do at compile time is collect the definitions for use
                 ; at runtime
                 [regroups (reverse groups)]
                 #;
                 [_ (println (list 'todo-list-groups groups))]
                 [dones? (member "|" groups)] ; FIXME use a grammar so we can error on multiple pipes
                 [dones (if dones? (cdr dones?) (list (car regroups)))]
                 [todos (reverse (if dones? (cdr (member "|" regroups)) (cdr regroups)))])
            ; FIXME do we need to error on duplicate keywords? probably?
            ; FIXME I think we also want a place to lookup the todo
            ; category so that we can embed it in the ast?
            ; FIXME not clear we need to remove duplicates for the heading parser
            (runtime-todo-keywords (remove-duplicates (append todo-keywords (runtime-todo-keywords))))
            (runtime-todo-keywords-changed #t)
            #;
            (println (list 'todo-line-stuff groups dones todos))
            #`(todo-cycle
               #:todo '#,todos ; FIXME preserve the syntax location
               #:done '#,dones))]
         [else #'(list 'keyword-line line)]))
     ]))

(define (todo-cycle #:todo [todo '()]
                    #:done [done '()])
  ; TODO this needs to expand to populate a runtime value, even if we also do it at syntax time?
  ; XXX changing todo keywords would necessitate a reparse, but I think only of the headings?
  (void))

(define-syntax (kw-prefix stx)
  ; XXX temporary implementation detail to compensate for e.g. #+title being a token
  (syntax-parse stx
    [(_ with-prefix:str)
     #:with without-prefix (datum->syntax #'with-prefix (substring (syntax-e #'with-prefix) 2))
     #'(keyword-key without-prefix)]))

#;
(define-syntax (todo-spec-line stx)
  ; XXX TODO this is where we have to bind the runtime todo keywords
  (syntax-parse stx
    [(_ line:str)
     (println #'line)
     ; at this phase we only need
     (runtime-todo-keywords (cons (runtime-todo-keywords)))
     #'(cons 'todo-spec-line line) ; XXX TODO
     ]))

;; table

(define-for-syntax (do-table str)
  (let* ([table-input (with-newlines str)]
         #;
         [__ (println (cons 'do-table-input: table-input))]
         [out (parse-table-to-datum ; TODO fixup srcloc in errors errors here probably?
               (table-make-tokenizer
                (open-input-string table-input)))])
    (when (debug)
      (pretty-write (cons 'do-table: out)))
    out))

(define-syntax (table-element stx)
  (syntax-parse stx
    [(_ table-string:str (~optional newline:str #:defaults ([newline #'""])))
     #:do [(when (debug) (println (list "table-element in:" (syntax-e #'table-string))))]
     #:with table
     (local-expand
      (datum->syntax
       #'table-string ; FIXME we can get better granularity than this surely ?
       (do-table (string-append (syntax-e #'table-string) (syntax-e #'newline))))
      'expression
      #f)
     (let ([out #'table])
       (when (debug)
         (pretty-write (cons 'table: (syntax->datum out))))
       out)
     ])
  )

(define-syntax (table-cell stx) ; FIXME abstract along with h-title
  (syntax-parse stx
    [(_ body ...)
     #:do [(when (debug) (println (list "table-cell in:" (syntax-e #'(body ...)))))
           (define table-cell-input
             (apply string-append
                    (map (λ (e)
                           (syntax->datum
                            (local-expand e 'expression #f)))
                         (syntax->list #'(body ...)))))]
     #:with (expanded ...) (local-expand ; FIXME I'm guessing that this loses the syntax provenance
                            (datum->syntax
                             #'(body ...)
                             (do-paragraph table-cell-input this-syntax))
                            'expression #f)
     #'(list 'table-cell expanded ...)]))
