#lang brag

heading : stars @heading-content? /wsnn* ( h-tags | NEWLINE-END )

heading-content : todo-keyword
                |                     h-priority
                |                                       h-comment
                |                                                        h-t-rpc-t
                | todo-keyword /wsnn* h-priority
                |                     h-priority /wsnn* h-comment
                | todo-keyword /wsnn*                   h-comment
                | todo-keyword /wsnn*                                     h-t-pc-t
                |                     h-priority /wsnn*                    h-t-c-t
                |                                       h-comment /wsnn*               h-title
                | todo-keyword /wsnn* h-priority /wsnn* h-comment
                | todo-keyword /wsnn* h-priority /wsnn*                    h-t-c-t
                |                     h-priority /wsnn* h-comment /wsnn*               h-title
                | todo-keyword /wsnn*                   h-comment /wsnn*               h-title
                | todo-keyword /wsnn* h-priority /wsnn* h-comment /wsnn*               h-title

; convenience nodes to simplify remergin the full title
h-t-rpc-t : h-title-r-p-c @h-title?
h-t-pc-t :  h-title-p-c   @h-title?
h-t-c-t :   h-title-c     @h-title?

; h-title-p is not needed because if you can't have p you also can't have c
@h-title-r-p-c : ( BLANK | OTHER | OOPS | STARS )+
@h-title-p-c : ( h-title-r-p-c | todo-keyword )+
@h-title-c : ( h-title-p-c | h-priority )+
h-title : ( h-title-c | CHARS-COMMENT )+

stars : STARS ; | ( NEWLINE ASTERISK BLANK )
todo-keyword : RUNTIME-TODO-KEYWORD
h-priority : PRIORITY ; XXX priority spacing rules are inconsistent
h-comment : CHARS-COMMENT
h-tags : TAGS | archive
archive : BLANK ARCHIVE
wsnn : BLANK

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
