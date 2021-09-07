#lang racket/base

(require ; FIXME minimal racket no gui issues
 drracket/tool
 (only-in racket/unit unit import export)
 (only-in racket/class make-object)
 (only-in racket/draw color%)
 #;
 (only-in string-constants/string-constant dynamic-string-constant)
 (only-in
  framework
  color-prefs:add-color-scheme-entry
  color-prefs:add-to-preferences-panel
  color-prefs:build-color-selection-panel))

(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))))

; so it turns out if you just use hte right style prefix then the
; default token-sym->style will just work (namely
; racket:short-sym->style-name)
(define style-base "framework:syntax-color:scheme:")

(define org-faces ; see org-faces.el
  '(
    bullet
    malformed

    #|
    agenda-calendar-event
    agenda-calendar-sexp
    agenda-clocking
    agenda-column-dateline
    agenda-current-time
    agenda-date
    agenda-date-today
    agenda-date-weekend
    agenda-diary
    agenda-dimmed-todo-face
    agenda-done
    agenda-filter-category
    agenda-filter-effort
    agenda-filter-regexp
    agenda-filter-tags
    agenda-restriction-lock
    agenda-structure
    |#

    archived
    beamer-tag
    block
    block-begin-line
    block-end-line
    checkbox
    checkbox-statistics-done
    checkbox-statistics-todo
    cite
    cite-key
    clock-overlay
    code
    column
    column-title
    date
    date-selected
    default
    dispatcher-highlight
    document-info
    document-info-keyword
    document-title
    done
    drawer
    ellipsis
    footnote
    formula
    #|
    habit-alert-face
    habit-alert-future-face
    habit-clear-face
    habit-clear-future-face
    habit-overdue-face
    habit-overdue-future-face
    habit-ready-face
    habit-ready-future-face
    |#
    headline-done
    headline-todo
    hide
    indent
    inlinetask
    latex-and-related
    link
    list-dt
    macro
    meta-line
    #|
    mode-line-clock
    mode-line-clock-overrun
    |#
    priority
    property-value
    quote
    scheduled
    scheduled-previously
    scheduled-today
    sexp-date
    special-keyword
    table
    table-header
    tag
    tag-group
    target
    time-grid
    todo
    upcoming-deadline
    upcoming-distant-deadline
    verbatim
    verse
    warning

    ))

(define color-prefs-table
  (let ([constant-green (make-object color% 41 128 38)]
        [symbol-blue (make-object color% 38 38 128)])
    (append
     (for/list ([n (in-range 1 10)])
       (list
        (string-append style-base (format "org-level-~a"  n))
        symbol-blue
        (format "org-level-~a" n)
        #; ; we don't have these set up and registered yet, not clear we want/need them
        (dynamic-string-constant (string->symbol (format "org-level-~a" n)))))
     (for/list ([frag org-faces])
       (list
        (string-append style-base (format "org-~a" frag))
        symbol-blue
        (format "org-~a" frag))))))

(define (colorer-register-styles)
  (for ([entry color-prefs-table]);([n (in-range 1 10)])
    (let ([style (car entry)]
          #;
          [style (string-append style-base (format "org-level-~a"  n))])
      (color-prefs:add-color-scheme-entry
       (string->symbol style)
       "white"
       "black"
       #:style style))))

(define (add-coloring-preferences-panel)
  (color-prefs:add-to-preferences-panel
   "Org"
   (λ (parent)
     (for-each
      (λ (line)
        (let ([sym (car line)])
          (color-prefs:build-color-selection-panel 
           parent
           (string->symbol sym)
           sym
           (caddr line))))
      color-prefs-table))))

#; ; this might still come in handy
(drracket:modes:add-mode
   "Org mode"
   #;
   (string-constant racket-mode)
   (new text-mode%)
   #;
   (λ (text prompt-position) (racket:text-balanced? text prompt-position))
   (λ (text prompt-position) #f)
   ; FIXME vs #lang org
   (λ (l) (member (string-constant module-language-name) l)))
(colorer-register-styles)
(add-coloring-preferences-panel)
