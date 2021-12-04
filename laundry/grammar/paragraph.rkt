#lang brag

paragraph : markup? ( paragraph-common
                    | stuff
                    | left-common
                    ;| malformed ; XXX note that if malformed is placed here it MUST terminate the paragraph
                    ;| sigh
                    )* ( footnote-inline-malformed-eof
                       | script-malformed-eof
                       ; | open-delim ; can't do this because it introduces ambiguity
                       )? newline ; markup-eof? ; markup-eof no longer needed and no tokens are being produced

@left-common : LSB script?
             | LCB script?
             | LP script?
             | LSB-PLUS script? ; XXX not sure if this is actually safe ?
; I'm fairly certain that the current state of this grammar has
; many hidden failure modes where certain strings simply cannot
; be expressed need to fuzz or far better, switch to using a syntax
; that does not involve nesting, nesting is an awful choice for the
; surface syntax of a markup language


@paragraph-common : mu-pre-less-whitespace ( markup | script )
                  | whitespace markup
                  | UNDERSCORE ; tokenizer handles all combined cases for this but it still has to be broken out
                  | object ( mu-free | script )?
                  | WSNN

@paragraph-common-open : paragraph-common | nested-delims | open-delim

;;; nested delimiter things

; XXXXXXXXXXXXXXXX having fiddled with this now, I strongy suggest
; that inline blocks like this switch to use shortest match and add
; support for something like racket's at expression syntax src_elisp|<{}>|
; which would avoid ambiguity altogether while ensuring that users
; can always represent what they need, this sandboxes the complexity
; into just the delimiters TODO implement this
@nested-delims : nested-square | nested-curlie | nested-parens
; TODO also need to handle [fn:: [cite: and any other cases
@nested-square : ( LSB | LSB-PLUS ) ( nested-square | paragraph-common | @stuff-less-rsb )* RSB
@nested-curlie : LCB ( nested-curlie | paragraph-common | @stuff-less-rcb )* RCB
@nested-parens : LP  ( nested-parens | paragraph-common | @stuff-less-rp  )* RP

@open-delim : ( LSB | LSB-PLUS ) ( paragraph-common-open | @stuff-less-rsb )*
            | LCB ( paragraph-common-open | @stuff-less-rcb )*
            | LP  ( paragraph-common-open | @stuff-less-rp  )*

; notes on whether certain things can be done entirely during tokenization
; nested structures that are not shortest match (i.e. markup) such as inline
; footnotes and scrip blocks cannot use a chained parsing approach without
; going through a parser, TODO we might be efficient if we could provide
; access to the original port during expansion so that we could read the
; string again instead of having to do a bunch of operations to reconstruct
; what it used to be
@object : latex-entity-fragment ; FIXME not clear that all of these appear in paragraph
        | latex-fragment
        | export-snippet
        | citation
        | footnote-reference ; FIXME can this be inside markup?
        | inline-call
        | inline-call-malformed
        | inline-src-block
        ; | line-break ; TODO
        | hyperlink ; aka links
        | macro
        | noweb-target ; aka target
        | radio-target
        | stats-cookie
        | script ; XXX TODO #+options: likely needs to be able to modify the tokenizer
        ; note: tabel cells are included in this list in the spec but do not appear at all in this parser
        | timestamp
        ; note: markup is also listed here in the spec but for now has a special place given its behavior

@stuff-base : STUFF-B ( mu-free | script )?
            | STUFF-A
            | STUFF-C script?
            | SCRIPT-DISABLED script?
            ;| LSB mu-free? script?
            ;| LCB script? ; FIXME mismatch I htink
            ;| LP script?  ; FIXME mismatch I htink
            | mu-pre
            ;| WSNN ; FIXME will this fight with mu-pre or no ?
            | newline

@stuff-less-rsb-1 : stuff-base | RCB mu-free? | RP mu-free?
stuff-less-rsb : stuff-less-rsb-1+
@stuff : ( stuff-less-rsb-1 | RSB mu-free? )+

@stuff-less-rcb-1 : stuff-base | RSB mu-free? | RP mu-free?
stuff-less-rcb : stuff-less-rcb-1+

@stuff-less-rp-1 : stuff-base | RCB mu-free? | RSB mu-free?
stuff-less-rp : stuff-less-rp-1+

newline : NEWLINE

;; objects

latex-entity-fragment : LATEX-ENTITY-OR-FRAGMENT-1

latex-fragment : latex-fragment-n | latex-fragment-parens
latex-fragment-n : LATEX-FRAGMENT-N
latex-fragment-parens : LATEX-FRAGMENT-PARENS

export-snippet : EXPORT-SNIP

citation : CITATION

@nested-lol : nested-square | footnote-inline ; this is the current elisp behavior ...

inline-call : INLINE-CALL-START
            ( nested-parens
            | nested-lol
              nested-parens
              nested-lol )

inline-call-malformed : INLINE-CALL-START
                      ( open-delim
                      | nested-lol open-delim
                      | nested-lol nested-parens open-delim
                      )? ; XXX the ? introduces ambiguity

; trailing headers are too useful, so []()[] is the other option
; the leading [] is missing then it wont parse, there isn't really any other way
;            nested-square? nested-parens
; the ambiguity here is a killer for the trailing header case
;            ( nested-square? nested-parens nested-square
;            | nested-square nested-parens nested-square
;            | nested-parens )

inline-src-block : INLINE-SRC-BLOCK-START ( nested-square | nested-lol )? nested-curlie

;;; line-break ; TODO not quite sure how to deal with this one right now

hyperlink : link-regular | link-angle
link-regular : LINK
link-angle : LINK-AB

macro : MACRO

noweb-target : NOWEB-TARGET

radio-target : RADIO-TARGET

stats-cookie : stats-percent | stats-quotient
stats-percent : STATS-PERCENT
stats-quotient : STATS-QUOTIENT

;;; sub super script

@script : subscript | superscript

subscript : SUBSCRIPT | subscript-bp

superscript : SUPERSCRIPT | superscript-bp

@subscript-bp : sub-b | sub-p
@sub-b : /SUB-START-B suffix-b
sub-p : /SUB-START-P suffix-p

@superscript-bp : sup-b | sup-p
@sup-b : /SUP-START-B suffix-b
sup-p : /SUP-START-P suffix-p

; FIXME not sure whether markup? can actually happen here for bol or bof yes, but for some of these
; nested cases I'm thinking probably not?
@suffix-b : suffix-b-pre /RCB
@suffix-p : suffix-p-pre /RP

@suffix-b-pre : markup? ( paragraph-common
                        | LCB ( paragraph-common | @stuff-less-rcb ) RCB
                        | LSB script?
                        | LP script?
                        | @stuff-less-rcb )*

@suffix-p-pre : markup? ( paragraph-common
                        | LP ( paragraph-common | @stuff-less-rp ) RP
                        | LSB script?
                        | LCB script?
                        | @stuff-less-rp )*

script-malformed-eof : ( SUP-START-B suffix-b-pre )*
                     | ( SUP-START-P suffix-p-pre )*

;;; timestamp 

timestamp : TIMESTAMP

;;; footnote

footnote-reference : footnote-anchor
                   | footnote-inline
                   ; | footnote-inline-simple
footnote-anchor : FOOTNOTE-ANCHOR
; FIXME RSB fighting with markup e.g. [fn:: hello =]= there ] vs [fn:: [ hello =]= there ]
paragraph-inline : @paragraph-inline-safe ;| paired-square
paragraph-inline-safe : markup? ( paragraph-common
                                | LSB ( paragraph-common | @stuff-less-rsb )* RSB
                                | @stuff-less-rsb )*

footnote-inline-malformed-eof : FOOTNOTE-START-INLINE paragraph-inline?

;paired-square : ( FOOTNOTE-START-INLINE | LSB ) stuff-less-rsb? RSB
;footnote-inline-simple : FOOTNOTE-INLINE-SIMPLE

; because footnote is longest match and because there is a collision with
; markup footnotes must be reparsed in a second pass, this is likely the case
; for script as well
footnote-inline : FOOTNOTE-START-INLINE paragraph-inline? /RSB ; inline footnotes may contain at most a single paragraph
                ;| FOOTNOTE-INLINE-MALFORMED-EOF ; FIXME wrong because it somehow ends with the banned value, sigh EOF madness
;malformed : footnote-inline-malformed
;footnote-inline-malformed : FOOTNOTE-INLINE-MALFORMED-MALFORMED
                          ; | FOOTNOTE-START-INLINE @stuff-less-rsb ; FIXME can't use due to ambig on rsb
; footnote-definition : FOOTNOTE-DEFINITION ; these never appear here, they are always outside paragraphs

;;; markup

; I think it is overall simpler to understand and implement parsing of markup
; in a second pass over blocks that have already been determined to be paragraphs
; trying to parse markup at the top level leads to a number of issues

; XXX LOL yeah newline *some text* is used all over the place and everywhere
; asterisk is pretty heavily used in here

;; sigh, again here grammar fails due to the need to match delimiters
;; the expresiveness of the ebnf is insufficient to be able to communicate
;; the fact that the starting marker must match the ending maker and then
;; just provide a list of markers, therefore we have to special case all of these

;; while it is technically possible in the elisp implementation to change markup
;; delimiters, this is something that has been ruled an implementation detail and
;; is not a feature of org, users modifying the concrete syntax are no longer using
;; org syntax but something else, therefore we do not support such functionality
;; despite the fact that in principle this could be implemented the same way as
;; runtime todo keywords are impelemented, however it adds significant complexity
;; to the tokenization step, however it may come to pass that this has to be implemented
;; in order to get correct behavior for #+options: in which case there isn't actually
;; any additional complexity since a conformant implementaiton would have to support
;; the ability to configure futher parsing in the zeroth section, however that might
;; be a second tier implementation, a first tier would only be required to handle
;; a specific set of defaults

@whitespace : ( WSNN | newline ) +
@mu-pre : ( mu-pre-less-whitespace | WSNN | newline )+
@mu-pre-less-whitespace : MU-PRE-SAFE | LCB | LP 
;@-mu-pre : ( MU-PRE-N-NOT-LCB | MU-PRE-1 | WSNN | newline )+
; FIXME check on interaction with pb-script

@markup : markup-rec | markup-terminal

markup-rec : bold | italic | underline | strike-through

markup-terminal : code | verbatim

bold : BOLD
italic : ITALIC
underline : UNDERLINE
strike-through : STRIKE
code : CODE
verbatim : VERBATIM

@mu-free : BOLD | ITALIC | UNDERLINE | STRIKE | VERBATIM ; x/a b/ like cases
