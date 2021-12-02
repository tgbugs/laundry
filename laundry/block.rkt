#lang brag

; NOTE greater blocks that do not have a special status e.g. src blocks for babel
; can only be determined to be malformed during a later pass, not by the grammar alone
block-begin-line : /newline wsnn* blk-greater-begin
block-end-line : /newline wsnn* blk-greater-end ; FIXME paragraph vs out of place end line

blk-ex : blk-ex-begin blk-ex-contents nlpws blk-ex-end
blk-ex-begin : BEGIN-EX wsnn blk-line-contents | BEGIN-EX
blk-ex-end : END-EX ; wsnn*? or we deal with that elsewhere ?
blk-ex-contents : no-headlines ; XXX watch out for greedy ?


; XXX NOTE these WILL mispair and have to be processed in a second step
; if this happens, the parse results up to the start of next heading cannot
; be trusted and should be considered to be incorrect
blk-greater-begin : blk-greater-type wsnn blk-line-contents | blk-greater-type

blk-greater-end : END-BLOCK block-type-name | bg-end-special
bg-end-special : END-SRC block-type-rest ; these are for things like #+end_srclol
               | END-EX block-type-rest

blk-greater-type : BEGIN-BLOCK block-type-name | bg-type-special
bg-type-special : BEGIN-SRC block-type-rest ; these are for things like #+begin_srclol
                | BEGIN-EX block-type-rest

blk-greater-malformed : ( END-BLOCK | BEGIN-BLOCK ) /wsnn* ; if this occures alone
block-type-name : @not-whitespace ;| CHARS-COMMENT | CHARS-ARCHIVE ; CHARS- are in not-whitespace via big-tokes
block-type-rest : @not-whitespace

;;; old

;@start-not-headline : @not-asterisk-newline1 | stars @not-asterisk-whitespace1 | @word-char-n

; I suspect that we can use complex | simple as a way to try to parse things that can fail
; that we do not what to fall through
;blk-line-contents : @not-newline
;blk-line-rest : @not-newline

;blk-src-malformed : SRC-BLOCK-MALFORMED

;blk-src : blk-src-whole ;| /newline blk-src-begin blk-src-contents? nlpws blk-src-end
;blk-src-whole : SRC-BLOCK ; XXX requires a nested parser OR chaining the positions of input port in the lexer to produce more than one token i.e. checking in next-token for a list
;blk-src : blk-src-begin newline blk-src-contents? wsnn* blk-src-end
;blk-src-begin : ( BEGIN-SRC wsnn blk-src-line-contents | BEGIN-SRC ) /wsnn*
;blk-src-begin-malformed : BEGIN-SRC /wsnn* newline ; FIXME newline first grammar issues :/
;blk-src-end : END-SRC /wsnn*
;blk-src-contents : no-headlines ; FIXME we just need the bounds for the contents of the block for 2nd pass?
;blk-src-contents : no-headlines-hungry-no-src-end ; this is a version that eats newlines
;blk-src-line-contents : language wsnn blk-src-line-rest-alt | language wsnn*
;language : @not-whitespace ; FIXME

;blk-unknown : UNKNOWN-BLOCK

;no-headlines-hungry-no-src-end : line-not-headline-not-src-end+
;line-not-headline-not-src-end : newline* start-not-headline not-newline-not-src-end newline

;;; switches XXX

; still evil but these are a little less evil

; switches and headlines are probably tied for the most aweful parts of the whole grammar

; the drafts of this section of the grammar before I removed the
; special case for -l were probably the inspiration for some of the
; more sanity destorying pages of the Necronomicon

; these should be removed from org-mode ASAP and replaced
; ideally with header arguments which are general and extensible
; before that can happen there needs to be an upgrade path for existing users

;blk-src-line-rest-alt : switches-sane /wsnn blk-src-args-after-switches-sane
                      ;| switches-sane
                      ;| @not-switch @not-newline?

;@--test--switches-sane : switches-sane
;switches-sane : ( switch-sane ( /wsnn format-string )? /wsnn )* switch-sane ( /wsnn format-string )?
;switch-sane : switch-sign alpha
;last-switch-string : wsnn format-string

;@switch-sign : PLUS | HYPHEN

;not-switch : @not-plus-hyphen1
;| PLUS   @not-alpha-newline1
;| HYPHEN @not-alpha-newline1
;| PLUS   @alpha @not-whitespace1
;| HYPHEN @alpha @not-whitespace1

;blk-src-parameters : COLON not-newline ; TODO
; NOTE the correct parameters grammar is a subset of
; the not-a-format-string grammar since it starts with colon
                   ;| wsnn+ COLON not-newline ; TODO

;@blk-src-args-after-switches-sane : blk-src-parameters
;                                  | @not-dq-ph-newline not-newline? ; TODO not-dq-ph-colon-newline for params
;                                  | switch-sign not-alpha-newline1 not-newline?
;                                  | switch-sign ( @alpha @not-whitespace | @word-char-n ) @not-newline?
;                                  | blk-src-args-broken ; XXX user done goofed

; you may have a dq there but then no more dq at all on the line
; this will be extremely weird, but could happen
;blk-src-args-broken : DQ @not-dq-newline?

;format-string : /DQ format-string-contents /DQ
; TODO other escape sequences
;format-string-contents : ( @not-bs-dq-newline | /BS DQ | BS )+

;string : DQ string-contents DQ
; TODO other escape sequences
;string-contents : ( @not-bs-dq | /BS DQ | BS )+

; make tests pass
wsnn : NOP
newline : NOP
nlpws : NOP
blk-line-contents : NOP
no-headlines : NOP
not-whitespace : NOP
