#lang brag

; conceptually we would like to be able to use the following definition for table
; table : ( /NEWLINE wsnn1-n? ( table-row | table-row-rule ) )+ /NEWLINE?
; however there is hidden quadratic behavior because there are optional elements inside table-row

table : table-nlf

;table : table-nll

table-nll : /NEWLINE ( table-row-rule | table-row-variants-nll )

@table-row-variants-nll : ( table-row-degenerate | table-row-open | table-row )

table-nlf : ( ( table-row-variants
              | table-row-rule
              ) )+ /NEWLINE?

;@start : /NEWLINE /wsnn1-n?
@table-row-variants : ( /NEWLINE | /NLWS ) ( table-row-degenerate | table-row-open | table-row )

; we special case this to avoid using ? in the definition of table-row
table-row-degenerate : table-cell-degenerate
table-cell-degenerate : /PIPE

table-row : table-cell+ /PIPE ; ? or table-cell+ alone induces the quadraticness
          ; | table-cell-degenerate

table-row-open : table-cell+

;table-row-before-rule : table-cell+ ; the table row rule token is unavoidably new line first

table-row-rule : TABLE-ROW-RULE ; /NEWLINE? ; /NEWLINE? is safe, needed for multiple rule rows in a ... row (lol)

table-cell : /PIPE ( REST | ESC-PIPE | BS | wsnn1-n )+
           | /PIPE

@wsnn1-n: WSNN1-N+
