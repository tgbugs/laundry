#lang brag

;-plain-list-line : /newline pl-indent ( ordered-list-line | descriptive-list-line )
;pl-indent : @wsnn*
;-ordered-list-line : bullet-counter plain-list-line-tail?
;bullet-counter : ( digits | alphas-unmixed ) ( PERIOD | R-PAREN )
;; XXX NOTE the worg spec is inconsistent with behavior, single letters DO NOT WORK AS COUNTERS
;; TODO ARGH, I've never checked what the org-export backends do O_O AAAAAAAAAAAAAAAAAAAA
;; yet another part of the elisp implementation to deal with FFS
;; html and tex export backends do not support letters in the bullet counter

;-descriptive-list-line : bullet-plain plain-list-line-tail?
;bullet-plain : HYPHEN | PLUS | wsnn+ ASTERISK 

;plain-list-line-tail : @wsnn+ @not-newline | @wsnn+ ; sadly we have to use the dumb parser here due to ambiguity
;plain-list-line-tail : wsnn+ counter-set? check-box? plain-list-tag? not-newline | wsnn
; TODO this can be negated I think

plain-list-yes-we-can : counter-set check-box plain-list-tag not-newline?
                      | counter-set           plain-list-tag not-newline?
                      |             check-box plain-list-tag not-newline?
                      |                       plain-list-tag not-newline?
                      | counter-set check-box pl-not-tag?
                      |             check-box pl-not-tag?
                      |             check-box
                      | counter-set pl-not-check-box?
                      | pl-not-counter-set not-newline? ; technically 

;                     | counter-set check-box plain-list-tag not-newline?

pl-not-counter-set : not-lsb-at-digits-newline
                   | LSB not-at-newline1
                   | LSB AT not-digits-newline
                   | LSB AT digits not-rsb-newline

pl-not-check-box : pl-not-check-box-start not-newline?

pl-not-check-box-start : not-lsb-space-X-hyphen-newline
                       | LSB not-space-X-hyphen-newline1
                       | LSB space-X-hyphen not-rsb-newline1

space-X-hyphen : space | CHAR-UPPER-X | HYPHEN

; XXX spec is unclear and I can't find anything obvious in org-element
; essentially you can't have two colons appear anywhere on the line, I'm not sure whether
; the plain-list-tag can be empty, but since I'm moving to allow empty tags for regularity
; it is consistent to allow empty tags here, also the plain list tag does NOT follow the
; same syntax as headline tags ... it is literally anything until the last paired colons on the line
pl-not-tag : not-colon-newline
           | ( not-colon-newline? COLON )+ not-colon-newline?

counter-set: LSB AT counter-set-content RSB  ; wow this is useful!
counter-set-content : digits
check-box : LSB ( cb-todo | cb-in-process | cb-done ) RSB
cb-todo : space
cb-in-process : HYPHEN ; started ?
cb-done : CHAR-UPPER-X ; AAAAAAAAAAAAAAAAAAAAAAAA ;_; complexity XXX

; TODO check if this taints lowercase as well
plain-list-tag-text : @not-newline
plain-list-tag : pl-tag-end
pl-tag-end : COLON COLON

; make tests pass
not-newline : NOP
not-rsb-newline : NOP
digits : NOP
not-digits-newline : NOP
not-at-newline1 : NOP
not-lsb-at-digits-newline : NOP
not-rsb-newline1 : NOP
not-space-X-hyphen-newline1 : NOP
not-lsb-space-X-hyphen-newline : NOP
space : NOP
not-colon-newline : NOP
