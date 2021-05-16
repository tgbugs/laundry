#lang brag

paragraph : markup? ( mu-pre markup
                    | stuff
                    | macro
                    | hyperlink )* markup-eof?

stuff : ( STUFF-B mu-free? | STUFF-A | MARKER | mu-pre | newline )+

newline : NEWLINE

;;;; */_+=~

;;; macros

macro : MACRO

;;; hyperlink

hyperlink : link-regular | link-angle
link-regular : LINK
link-angle : LINK-AB

;;; markup

@mu-pre : ( MU-PRE-N-NOT-LCB | MU-PRE-1 )+
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
@markup : markup-rec | markup-terminal
markup-rec : bold | italic | underline | strike-through
         ;| bold-italic | bold-underline | bold-strike-through
         ;| italic-underline | italic-strike-through | underline-strike-through
         ;| bold-italic-strike-through | bold-italic-underline
         ;| bold-underline-strike-through | italic-underline-strike-through
         ;| bold-italic-underline-strike-through) @mu-post?

markup-terminal : code | verbatim

bold : BOLD
italic : ITALIC
underline : UNDERLINE
strike-through : STRIKE
code : CODE
verbatim : VERBATIM

@mu-free : BOLD | ITALIC | UNDERLINE | STRIKE | VERBATIM ; x/a b/ like cases

markup-eof : eof-bold | eof-italic | eof-underline | eof-strike-through | eof-code | eof-verbatim

eof-bold : BOLD-EOF
eof-italic : ITALIC-EOF
eof-underline : UNDERLINE-EOF
eof-strike-through : STRIKE-EOF
eof-code : CODE-EOF
eof-verbatim : VERBATIM-EOF

;mu-post : MU-PRE-1

; PRE MARKER CONTENTS MARKER POST
mu-post-old : SPACE
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
        | NEWLINE

;mu-pre-common : HYPHEN | L-PAREN | LCB | SQ | DQ
;mu-pre : HYPHEN | L-PAREN | LCB | SQ | DQ | whitespace
;mu-pre-no-newline : HYPHEN | L-PAREN | LCB | SQ | DQ | wsnn

;mu-marker : "*" | "/" | "_" | "+" | "=" | "~"
;mu-post : "-"|"."|","|";"|":"| "!"| "?"| "'" | L-PAREN | LCB | RSB | DQ
; up to 1 newlines and any object in a paragraph but only for */+_ not ~=
;mu-content : mu-border not-newline ( newline not-newline )? mu-border
;mu-border : not-whitespace
;mu-contents-cv : not-newline ( newline not-newline )?

;bold-italic : MU-BI
;bold-underline : MU-BU
;bold-strike-through : MU-BS
;italic-underline : MU-IU
;italic-strike-through : MU-IS
;underline-strike-through : MU-US

;bold-italic-strike-through : MU-BIS
;bold-italic-underline : MU-BIU
;bold-underline-strike-through : MU-BUS
;italic-underline-strike-through : MU-IUS

;bold-italic-underline-strike-through : MU-BIUS
