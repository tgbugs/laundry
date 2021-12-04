#lang brag

table : ( /NEWLINE /wsnn* table-row | table-row-rule )+ /NEWLINE?
table-row-rule : TABLE-ROW-RULE
table-row : table-cell+ /PIPE? ; FIXME PIPE? is technically ambiguous
table-cell : /PIPE /wsnn* ( REST | ESC-PIPE | BS | @wsnn )* /wsnn* ; FIXME @wsnn is ambiguous so have to strip ?
;table-row : table-cell-first table-cell-rest* /PIPE?
; XXX divergence due to being able to escape pipes
;@table-cell-first : /PIPE ( ( /wsnn+ | not-pbhn ) ( not-pipe-bs-newline | ESC-PIPE | BS )* )? /wsnn*
;@table-cell-rest : /PIPE /wsnn* ( not-pipe-bs-newline | ESC-PIPE | BS )* /wsnn*
;table-row-rule : /PIPE /HYPHEN /not-newline* ; everything else gets wiped XXX but careful with roundtrips?

wsnn: space | tab
;@not-pbhn : REST | space | tab
;@not-pipe-bs-newline : REST | space | tab | HYPHEN
;not-newline : REST | space | tab | HYPHEN | PIPE | BS

; have to have explicit nodes for these since they aren't lexemes
tab : TAB
space : SPACE
