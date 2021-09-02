#lang brag

; So. Parsing titles is hard.

; In the first draft of this grammar I made the exact same mistake as
; the elisp implementation. The mistake is to eat the space that
; terminates stars rather than peeking it. This is from/to vs
; from/stop-before. The tokenizer has to deal with this.
heading : stars heading-rest

@heading-rest : ( /wsnn+ @heading-content )? ( h-tags | NEWLINE-END )
              | /wsnn+ NEWLINE-END

; Note that unlike the other elements h-priority does not have to be followed by a space,
; however tags must still be preceeded by a space
heading-content : todo-keyword /wsnn*
                |                     h-priority /wsnn*
                |                                       h-comment /wsnn*
                |                                                         h-t-rpc-t
                | todo-keyword /wsnn+ h-priority /wsnn*
                |                     h-priority /wsnn* h-comment /wsnn*
                | todo-keyword /wsnn+                   h-comment /wsnn*
                | todo-keyword /wsnn+                                      h-t-pc-t
                |                     h-priority /wsnn*                     h-t-c-t
                |                                       h-comment /wsnn+               h-title
                | todo-keyword /wsnn+ h-priority /wsnn* h-comment
                | todo-keyword /wsnn+ h-priority /wsnn*                     h-t-c-t
                |                     h-priority /wsnn* h-comment /wsnn+               h-title
                | todo-keyword /wsnn+                   h-comment /wsnn+               h-title
                | todo-keyword /wsnn+ h-priority /wsnn* h-comment /wsnn+               h-title

; convenience nodes to simplify remerging the full title
h-t-rpc-t : h-title-r-p-c @h-title? ; in this case if you don't have a todo-keyword first then anything after is not comment or priority even if they look like they are
h-t-pc-t :  h-title-p-c   @h-title?
h-t-c-t :   h-title-c     @h-title?

; h-title-p is not needed because if you can't have p you also can't have c
; BLANK is excluded from being the first char since a single blank will match
; allowing h-title to incorrectly match COMMENT or [#A]
@h-title-r-p-c : ( BLANK | OTHER | OOPS | STARS
                 | CHARS-COMMENT @not-blank
                 | RUNTIME-TODO-KEYWORD ( PRIORITY | CHARS-COMMENT | @not-blank ) )+ ; this can't be * must be +
@h-title-p-c : ( h-title-r-p-c | @todo-keyword )+ ; FIXME misplaced ?
@h-title-c : ( h-title-p-c | @h-priority )+ ; FIXME misplaced ?
h-title : ( h-title-c | @h-comment )+ ; FIXME misplaced ?

stars : STARS ; | ( NEWLINE ASTERISK BLANK )
todo-keyword : RUNTIME-TODO-KEYWORD
h-priority : PRIORITY ; XXX priority spacing rules are inconsistent
h-comment : CHARS-COMMENT
h-tags : TAGS ; | /wsnn+ archive
; archive : ARCHIVE /wsnn* /NEWLINE-END
wsnn : BLANK

not-blank : OTHER | OOPS | STARS | PRIORITY

--test--heading-rest : heading-rest
; 1
; hr
;    hp
;       hc
;          ht-r-p-c ht?
; 2
; hr hp
;    hp hc
; hr    hc
; hr       ht-p-c   ht?
;    hp    ht-c     ht?
;       hc          ht
; 3
; hr hp hc
; hr hp    ht-c     ht?
;    hp hc          ht
; hr    hc          ht
; 4
; hr hp hc          ht

; heading-sigh : stars (todo-keyword wsnn)? (priority wsnn)? (comment wsnn)? title tags?

; heading-content-sigh : todo-keyword (/wsnn+ h-mid)?
;                      | mid-no-rtk?
;                      | mid-weird?
;                      | mid-no-rtk
;                      | tags-tail?



; h-mid : h-priority /wsnn* h-comment h-title?
;       | h-priority /wsnn* h-title-no-comment?
;       ; TODO I think we need to unnegate lsb at some point here?
;       | h-comment h-title-no-priority?
;       | h-title-no-priority-no-comment



; XXX as usual you have to handle all the combinations individually
; you can't just make them optional because the title contents are
; conditional
; h-title : OTHER | OOPS | BLANK
; h-title-no-rtk : OTHER | OOPS | BLANK
; h-title-with-rtk
; h-title-first :
; h-title-rest : @title-first | CHARS-COMMENT
