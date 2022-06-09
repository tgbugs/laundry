#lang brag

; this is not the right approach for implementation
; you need a principled approach that goes special by special and
; enumerates every possibility for interaction with other specials :/
; what a mess ... context sensitive lexing seems like it would be
; way easier ... except that that is just hand rolling the state machine :/

paragraph : ( whitespace? paragraph-parts whitespace? )+ ( malformed | undersigh )? whitespace?
          | whitespace? ( malformed | undersigh | not-script ) whitespace?
          ;| whitespace? not-markup whitespace?

@not-markup : ASDF
            | ( bbs | other ) ( mu-marker-free | UNDERSCORE | UNDERSCORE-SCRIPT-DISABLED )
            | ( mu-marker-free | UNDERSCORE | UNDERSCORE-SCRIPT-DISABLED )
            | ( mu-marker-free | UNDERSCORE | UNDERSCORE-SCRIPT-DISABLED ) ( mu-marker-free | UNDERSCORE | UNDERSCORE-SCRIPT-DISABLED )

@paragraph-parts : paragraph-common
                 | @bracket
                 | @bracket mu-marker-free ; FIXME too many special cases here ...
                 | @bracket script+
                 | ( UNDERSCORE | UNDERSCORE-SCRIPT-DISABLED )+ script+
                 | undersigh ( object | nested-square | nested-paren) ; I think !?, can't use paragraph-common because it has nested as a constituent now which would induce ambiguity?
                 | whitespace @left-bracket
                 ;| LAB | RAB ; ok because the only larger constructs are handled in the lexer
                 | whitespace ; newline newline case, trust the top level lexer to give us sane stuff ; XXX careful with this can't keep this in common due to issues with nesting of the terminal newline that we need to truncate? a stupid issue to have, but also technically in the spec, or rather, make sure malformed doesn't make it out of do-paragraph? FIXME no good answers here

@paragraph-common : ASDF ; bbs now in object via plain text
                  | todo
                  ;| markup ; now via object
                  | object
                  | nested ; I think this is safe because not-rxb is only intended to handle unmatched cases?

;@object- : hyperlink
;        | export-snippet
;        | citation
;        | timestamp
;        | macro
;        | fotnote-reference
;        ;| script must be preceeded by not whitespace so can't go here

@object : object-standard
        | citation

@plain-text : plain-text-1+
@plain-text-1 : other ; TODO I'm sure I'm missing something in here obviously missing any and all brackets, which we might actually be able to include?
              | bbs
              | mu-marker-free
              ;| not-markup ; this is now safe because the lexer is now context aware ; FIXME TODO, this means we should just be able to throw mu-marker-free in directly without issue? XXX FALSE only mu-marker-free is safe, otherwise this gobbles underscores which we need for script
              | wsnn1
              | script-disabled+ ; sort of an object, but really the current implementation treats it as just more text
              | LP ; XXX somehow I think these are safe to be unmached now?
              | RP
              | LAB
              | RAB
              | AT
              ; I think these are safe now that we have _{ and ^{ in the lexer
              | HAT
              | HAT-SCRIPT-DISABLED
              | UNDERSCORE
              | UNDERSCORE-SCRIPT-DISABLED


@object-standard : ASDF
        | latex-entity-fragment ; FIXME not clear that all of these appear in paragraph
        | latex-fragment
        | export-snippet
        | footnote-reference ; FIXME can this be inside markup?
        | inline-call
        | inline-src-block
        | hyperlink ; aka links
        | macro
        | noweb-target ; aka target
        | radio-target
        | stats-cookie
        | timestamp
        ; according to the spec
        | markup
        | plain-text
        | script ; context sensitive lexer enables this I think? FIXME but fights with underline
;        | inline-call-malformed
;        ; | line-break ; TODO
;        ;| script ; XXX TODO #+options: likely needs to be able to modify the tokenizer ; script cannot go in as object, because objects can be preceeded by whitespace! (DUH)
;        ; note: tabel cells are included in this list in the spec but do not appear at all in this parser
;        ; note: markup is also listed here in the spec but for now has a special place given its behavior


@nested-lol : nested-square | footnote-inline ; probably a bug in the current elisp?

inline-call : INLINE-CALL-START ( nested-paren | ( nested-lol nested-paren nested-lol )) ; TODO specialize nested-curlie and friends for this

inline-src-block : INLINE-SRC-BLOCK-START ( nested-lol )? nested-curlie ; TODO specialize nested-curlie and friends for this

; todo
@todo : ASDF
     | wsnn1 not-script
     | newline not-script
     ;| not-script-not-underline
     | other script+
     | other
     | other            mu-marker-free
     | script-disabled+ mu-marker-free
     |                  mu-marker-free wsnn1
     | mu-hrm wsnn1
     | mu-hrm

;todo-uncommon : bracket mu-marker-free

malformed : malformed-beg malformed-end?
          ;| FOOTNOTE-START-INLINE LSB ; this matches, but by doesn't
          ;| not-script-not-underline
@malformed-end : ( other | bbs )+
@malformed-beg : FOOTNOTE-START-INLINE ( newline? ( paragraph-common | nested-square | not-rsb ) )+ ; XXX MMMM destroy that perf
               | FOOTNOTE-START-INLINE not-rsb
               | FOOTNOTE-START-INLINE undersigh
               ;| FOOTNOTE-START-INLINE LSB ; HOW THE FOO DOES THIS MATCH WHEN not-rsb fails !??!?! missing the + idiot
               | FOOTNOTE-START-INLINE
               | ( UNDERSCORE | UNDERSCORE-SCRIPT-DISABLED ) LCB wrapped-script-contents?
               | LSB not-rsb?
               | LCB not-rcb?
               | LAB not-rab?
               | undersigh? RSB+ undersigh? ; only unterminated at the end, otherwise force match nested
               | undersigh? RCB+ undersigh? ; only unterminated at the end, otherwise force match nested

@undersigh : ( UNDERSCORE | UNDERSCORE-SCRIPT-DISABLED )+

@not-thing-base : bbs | other | whitespace

@ntb-with-lbs : not-thing-base | left-bracket

@not-rsb-1 : ntb-with-lbs | right-bracket-less-rsb
@not-rcb-1 : ntb-with-lbs | right-bracket-less-rcb
@not-rab-1 : ntb-with-lbs | right-bracket-less-rab
@not-rp-1  : ntb-with-lbs | right-bracket-less-rp

@not-rsb : not-rsb-1+
@not-rcb : not-rcb-1+
@not-rab : not-rab-1+
@not-rp  : not-rp-1+

@not-lsb-rsb-1 : not-thing-base | left-bracket-less-lsb | right-bracket-less-rsb
@not-lcb-rcb-1 : not-thing-base | left-bracket-less-lcb | right-bracket-less-rcb
@not-lab-rab-1 : not-thing-base | left-bracket-less-lab | right-bracket-less-rab
@not-lp-rp-1   : not-thing-base | left-bracket-less-lp  | right-bracket-less-rp

@not-lsb-rsb : not-lsb-rsb-1+
@not-lcb-rcb : not-lcb-rcb-1+
@not-lab-rab : not-lab-rab-1+
@not-lp-rp   : not-lp-rp-1+

@left-bracket           : LSB | LCB | LAB | LP
@left-bracket-less-lsb  :       LCB | LAB | LP
@left-bracket-less-lcb  : LSB |       LAB | LP
@left-bracket-less-lab  : LSB | LCB |       LP
@left-bracket-less-lp   : LSB | LCB | LAB

@right-bracket :          RSB | RCB | RAB | RP
@right-bracket-less-rsb :       RCB | RAB | RP
@right-bracket-less-rcb : RSB |       RAB | RP
@right-bracket-less-rab : RSB | RCB |       RP
@right-bracket-less-rp  : RSB | RCB | RAB

bracket : LSB
| RSB
| LCB
| RCB
| LP
| RP
| LAB
| RAB

stuff : ASDF

@script-contents : OTHER | OTHERN ; FIXME not clear that OTHERN should actually be in script contents given that it indicates a break via alt?
                 | ASTERISK
                 | ( PLUS | MINUS ) ( OTHER | OTHERN ) ; , / .

;markup-pre : newline | wsnn1 | MU-PRE-SAFE
;markup-post : @markup-pre | MU-POST
underline-contents : ASDF
markup-contents : ASDF
markup-terminal-contents : ASDF
link-regular-link-contents : ASDF
link-regular-text-contents : ASDF
timestamp-square-contents : ASDF
@footnote-inline-contents : ( not-script | undersigh )? ( newline? ( paragraph-common | nested-square ) )+ ; XXX MMMM destroy that perf

link-angle-contents : ASDF
timestamp-angle-contents : ASDF


@nested : nested-square | nested-curlie | nested-paren

@nested-square : LSB ( nested-square | paragraph-common | @not-lsb-rsb )+ RSB
               | LSB RSB

@nested-curlie : LCB ( nested-curlie | paragraph-common | @not-lcb-rcb )+ RCB
               | LCB RCB

@nested-paren : LP ( nested-paren | paragraph-common | @not-lp-rp )+ RP
              | LP RP

footnote-reference : footnote-anchor
                   | footnote-inline

footnote-anchor : FOOTNOTE-ANCHOR

footnote-inline : FOOTNOTE-START-INLINE footnote-inline-contents /RSB
                | FOOTNOTE-START-INLINE /RSB
                ;| /LSB footnote-inline-contents /RSB ; FIXME sigh colon

; script
@script : superscript | subscript

@script-disabled : ( HAT-SCRIPT-DISABLED | UNDERSCORE-SCRIPT-DISABLED ) script-contents

@wrapped-script-contents : ( object-standard | not-lcb-rcb | nested )+

superscript : /HAT script-contents
             | ( /HAT | /HAT-SCRIPT-DISABLED ) /LCB wrapped-script-contents? /RCB
             | /SUP-START-B wrapped-script-contents? /RCB

subscript : /UNDERSCORE script-contents
           | ( /UNDERSCORE | /UNDERSCORE-SCRIPT-DISABLED ) /LCB wrapped-script-contents? /RCB
           | /SUB-START-B wrapped-script-contents? /RCB

@not-script : HAT script-contents
            | ( HAT | HAT-SCRIPT-DISABLED ) LCB wrapped-script-contents? RCB
            | UNDERSCORE script-contents
            | ( UNDERSCORE | UNDERSCORE-SCRIPT-DISABLED ) LCB wrapped-script-contents? RCB
            | SUP-START-B wrapped-script-contents? RCB
            | SUB-START-B wrapped-script-contents? RCB

; no longer needed due to proper use of contextual lexing I think
@not-script-not-underline : ( HAT
                            | HAT-SCRIPT-DISABLED
                            | UNDERSCORE
                            ; FIXME can't trail on the newline, it will things at the end of the line
                            | UNDERSCORE-SCRIPT-DISABLED ) ( newline | WSNN1 ) ; TODO more things not valid here

;; objects

latex-entity-fragment : LATEX-ENTITY-OR-FRAGMENT-1

latex-fragment : latex-fragment-n | latex-fragment-parens
latex-fragment-n : LATEX-FRAGMENT-N
latex-fragment-parens : LATEX-FRAGMENT-PARENS

export-snippet : EXPORT-SNIP
               | /AT /AT stuff /AT /AT ; FIXME do we need colon in there or can we do it in expansion?

citation : CITATION

hyperlink : link-regular | link-angle
link-regular : LINK
             | /LSB /LSB link-regular-link-contents /RSB /LSB link-regular-text-contents /RSB /RSB
link-angle : LINK-AB
           | /LAB link-angle-contents /RAB

macro : MACRO

noweb-target : NOWEB-TARGET

radio-target : RADIO-TARGET

stats-cookie : stats-percent | stats-quotient
stats-percent : STATS-PERCENT
stats-quotient : STATS-QUOTIENT

timestamp : TIMESTAMP

; markup

@mu-marker-free : ASTERISK | SLASH | PLUS | TILDE | EQUALS | UNDERSCORE | UNDERSCORE-SCRIPT-DISABLED

@markup : markup-manual
        | markup-rec | markup-terminal

markup-manual : underline-x | bold-x | italic-x | strike-x | code-x | verbatim-x

markup-rec : bold | italic | underline | strike-through
markup-terminal : code | verbatim

bold : BOLD
italic : ITALIC
underline : UNDERLINE
strike-through : STRIKE
code : CODE
verbatim : VERBATIM

; we cannot consume pre/post here it must live in the higher structure of the grammar
; which makes it hard to understand what is going on from a documentation point of view

marker-underline : UNDERSCORE | UNDERSCORE-SCRIPT-DISABLED

underline-x : /marker-underline @underline-contents /marker-underline

bold-x : /ASTERISK @markup-contents /ASTERISK

italic-x : /SLASH @markup-contents /SLASH

strike-x : /PLUS @markup-contents /PLUS

code-x : /TILDE @markup-terminal-contents /TILDE

verbatim-x : /EQUALS @markup-terminal-contents /EQUALS

newline : NEWLINE
@wsnn1 : WSNN1
@whitespace : ( NEWLINE | WSNN1 )+
@bbs : BEFORE-BEFORE-SPECIAL
@other : OTHER | OTHERN ; FIXME if this is not in place we hit contract errors on some string-append operation
mu-hrm : MU-PRE-SAFE | MU-POST

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; curly


; angle

timestamp-angle : /LAB timestamp-angle-contents /RAB
timestamp-square : /LSB timestamp-square-contents /RSB



; timestamp

timestamp- : TIMESTAMP
          | timestamp-square | timestamp-angle

; hyperlink

