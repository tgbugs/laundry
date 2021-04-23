#lang brag

paragraph-2 : ( markup | stuff )*

stuff : STUFF+ ; have to have + due to ambiguous max length of the lexer match

;;;; */_=~

;;; markup

; XXX I think it is overall simpler to understand and implement parsing of markup
; in a second pass over blocks that have already been determined to be paragraphs
; trying to parse markup at the top level leads to a number of issues since it is
; hard to get brag to peek and using from/stop-before gobbles headings etc.

; XXX LOL yeah newline *some text* is used all over the place and everywhere
; asterisk is pretty heavily used in here

;; sigh, again here grammar fails due to the need to match delimiters
;; the expresiveness of the ebnf is insufficient to be able to communicate
;; the fact that the starting marker must match the ending maker and then
;; just provide a list of markers, therefore we have to special case all of these

; | markup @not-newline?

;; NOTE FURTHER that MARKUP actually CANNOT BE PART OF THE GRAMMAR
;; because org files CAN DEFINE THEIR OWN MARKUP DELIMITERS!!!!!!!
markup : ( bold | italic | underline | strike-through | code | verbatim ) @mu-post?
; PRE MARKER CONTENTS MARKER POST
mu-post : SPACE
        | TAB
        | DASH
        | PERIOD
        | COMMA ; new
        | SC ; new
        | COLON
        | SQ
        | DQ
        | R-PAREN
        | RCB
        | LSB
        | BANG ; new
        | QM

;mu-pre-common : HYPHEN | L-PAREN | LCB | SQ | DQ
;mu-pre : HYPHEN | L-PAREN | LCB | SQ | DQ | whitespace
;mu-pre-no-newline : HYPHEN | L-PAREN | LCB | SQ | DQ | wsnn

;mu-marker : "*" | "/" | "_" | "+" | "=" | "~"
;mu-post : "-"|"."|","|";"|":"| "!"| "?"| "'" | L-PAREN | LCB | RSB | DQ
; up to 1 newlines and any object in a paragraph but only for */+_ not ~=
;mu-content : mu-border not-newline ( newline not-newline )? mu-border
;mu-border : not-whitespace
;mu-contents-cv : not-newline ( newline not-newline )?

bold : BOLD
italic : ITALIC
underline : UNDERLINE
strike-through : STRIKE
code : CODE
verbatim : VERBATIM
