#lang info

(define collection "org")

(define drracket-tools '("mode.rkt"))

(define deps '("base"
               "laundry"))

(define build-deps '())

;; syntax coloring

(define dark-green #(#x00 #x64 #x00))
(define better-blue #(#x20 #x20 #xff))
(define better-blue-2 #(#x00 #x40 #xff))
(define purple #(#xa0 #x20 #xf0))
(define steel-blue #(#x46 #x82 #xb4))
(define orange #(#xff #xa5 #x00))
(define dark-magenta #(#x8b #x00 #x8b))
(define dark-olive-green #(#x55 #x6b #x2f))
(define dark-cyan  #(#x00 #x8b #x8b))
(define gray-30 #(#x4d #x4d #x4d))
(define dark-orchid-1 #(#xbf #x3e #xff))
(define slate-blue-2 #(#x7a #x67 #xee))
(define light-sky-blue #(#x87 #xce #xfa))
(define deep-pink #(#xff #x14 #x93))
(define red #(#xff #x00 #x00))
(define cyan #(#x00 #xff #xff))
(define gold #(#xff #xd7 #x00))
(define pink #(#xff #xc0 #xcb))
(define pale-green #(#x98 #xfb #x98))

(define framework:color-schemes
  `(#hash((name . "Org")
          (white-on-black-base? . #t)
          (colors
           .
           ((framework:syntax-color:scheme:comment     ,deep-pink)
            (framework:syntax-color:scheme:constant    #(211 72 255))
            (framework:syntax-color:scheme:error       ,red)
            (framework:syntax-color:scheme:parenthesis #(0 150 255))
            (framework:syntax-color:scheme:string      ,cyan)

            (framework:syntax-color:scheme:org-malformed    ,gold)
            (framework:syntax-color:scheme:org-level-1 bold ,dark-green)
            (framework:syntax-color:scheme:org-level-2 bold ,better-blue)
            (framework:syntax-color:scheme:org-level-3 bold ,purple)
            (framework:syntax-color:scheme:org-level-4 bold ,steel-blue)
            (framework:syntax-color:scheme:org-level-5 bold ,orange)
            (framework:syntax-color:scheme:org-level-6 bold ,dark-magenta)
            (framework:syntax-color:scheme:org-level-7 bold ,dark-olive-green)
            (framework:syntax-color:scheme:org-level-8 bold ,dark-cyan)
            (framework:syntax-color:scheme:org-level-9 bold ,gray-30)

            (framework:syntax-color:scheme:org-meta-line        ,slate-blue-2)
            (framework:syntax-color:scheme:org-table            ,light-sky-blue)
            (framework:syntax-color:scheme:org-drawer           ,light-sky-blue)
            (framework:syntax-color:scheme:org-block-begin-line ,dark-orchid-1)
            (framework:syntax-color:scheme:org-block-end-line   ,dark-orchid-1)
            (framework:syntax-color:scheme:org-link             ,cyan)
            (framework:syntax-color:scheme:org-todo             ,pink)
            (framework:syntax-color:scheme:org-done             ,pale-green)

            (framework:syntax-color:scheme:org-bullet           ,light-sky-blue) ; XXX not in org-faces.el

            ; TODO big list
            (framework:syntax-color:scheme:org-cite             ,steel-blue)
            (framework:syntax-color:scheme:org-cite-key         ,steel-blue)
            (framework:syntax-color:scheme:org-footnote         ,steel-blue)

            ))
          )))
