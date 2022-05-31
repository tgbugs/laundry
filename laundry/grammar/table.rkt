#lang brag

; conceptually we would like to be able to use the following definition for table
; table : ( /NEWLINE wsnn1-n? ( table-row | table-row-rule ) )+ /NEWLINE?
; however there is hidden quadratic behavior because there are optional elements inside table-row

table : /NEWLINE ( wsnn1-n? ( table-row | table-row-rule | table-row-before-rule table-row-rule ) )+

; we special case this to avoid using ? in the definition of table-row
table-cell-degenerate : /PIPE-FINAL

table-row : table-cell+ ( /PIPE-FINAL | /NEWLINE ) ; ? or table-cell+ alone induces the quadraticness
          | table-cell-degenerate

table-row-before-rule : table-cell+ ; the table row rule token is unavoidably new line first

table-row-rule : TABLE-ROW-RULE /NEWLINE? ; /NEWLINE? is safe, needed for multiple rule rows in a ... row (lol)

table-cell : /PIPE
           | /PIPE ( REST | ESC-PIPE | BS | wsnn1-n )+

@wsnn1-n: WSNN1-N+
