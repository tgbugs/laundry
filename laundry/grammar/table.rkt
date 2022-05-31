#lang brag

table : /NEWLINE wsnn1-n? ( table-row | table-row-rule | table-row-before-rule table-row-rule )+

;( ( /NEWLINE | /PIPE-FINAL ) wsnn1-n? ( table-row | table-row-rule )? )+
;table : ( /NEWLINE wsnn1-n? ( table-row | table-row-rule ) )
      ;| ( /NEWLINE wsnn1-n? ( table-row | table-row-rule ) ) ( ( /NEWLINE | /PIPE-FINAL ) wsnn1-n? ( table-row | table-row-rule )? )+

;-table : ( /NEWLINE wsnn1-n? table-row | table-row-rule )+ /NEWLINE?

;table-row-degenerate : table-cell-degenerate
table-cell-degenerate : /PIPE-FINAL

table-row-rule : TABLE-ROW-RULE /NEWLINE? ; FIXME multiple table-row-rules in a row will break this
table-row-before-rule : table-cell+
table-row : table-cell+ ( /PIPE-FINAL | /NEWLINE ) ; XXX ()? or /NEWLINE? absolutely tanks performance, otherwise this is ALMOST linear
          ;| table-cell+  ; this also tanks performance
          | table-cell-degenerate
          ;| table-cell+ /PIPE ; I don't think this variant works due to ambiguity
;table-cell : /PIPE wsnn1-n? ( REST | ESC-PIPE | BS | wsnn1-n )* wsnn1-n? ; FIXME @wsnn is ambiguous so have to strip ?

table-cell : /PIPE
           | /PIPE ( REST | ESC-PIPE | BS | wsnn1-n )+
;table-row : table-cell-first table-cell-rest* /PIPE?
; XXX divergence due to being able to escape pipes
;@table-cell-first : /PIPE ( ( /wsnn+ | not-pbhn ) ( not-pipe-bs-newline | ESC-PIPE | BS )* )? /wsnn*
;@table-cell-rest : /PIPE /wsnn* ( not-pipe-bs-newline | ESC-PIPE | BS )* /wsnn*
;table-row-rule : /PIPE /HYPHEN /not-newline* ; everything else gets wiped XXX but careful with roundtrips?

@wsnn1-n: WSNN1-N+

;wsnn: space | tab
;@not-pbhn : REST | space | tab
;@not-pipe-bs-newline : REST | space | tab | HYPHEN
;not-newline : REST | space | tab | HYPHEN | PIPE | BS

; have to have explicit nodes for these since they aren't lexemes
;tab : TAB
;space : SPACE | SPACES
