; it is easier to navigate this file if you use `outline-minor-mode'
#lang brag

;;; this grammar is newline first NOT newline last

;; even with this limitation you can implement a
;; single pass parser for org-mode if you have a
;; tokenizer that allows you to peek a single char
;; ahead to spot newlines

;;;; introduction

; There are two ways to approach the grammar for org. One is as a
; complete grammar that tries to account all the details in the spec
; and sections of the file the other is a simplified one that deals
; almost entirely with single lines and rarely (in the case of tables,
; comments, and drawers) with groups of multiple lines
;
; IF you can enumerate all the cases by which a paragraph line
; may NOT start, then the grammar can be defined in a way that
; is order free, namely, that there will be no lurking implementation
; details that will/would affect how an org file is parsed depending
; on the order of the rules in the grammar
;
; for the basic elements this is already the case, it cannot be the
; case for headline-node however because there is no finite way
; to not match an arbitrary number of asterisks followed by whitespace
;
; This grammar was implemented using a tokenizer that does not have
; a notion of unicode, much of the complexity of the exiting grammar
; comes from using negative definitions in the grammar to ensure that
; it is closed.
;
; One potential advantage of this is that only the set of charachters
; known and used by the grammar need to be recognized and all of them
; are a single byte.

;;; meta

; how to write a grammar
; identify everything that you will need to negate
; create a tokenizer pattern that recognizes only the negation of that set
; create a tokenizer pattern that recognizes the individual elements that
; you need will need unnegated to assembed (hopefully less than) the full
; combinatorial covering of the less than negated space
; basically, you have to cut out your hole, and then fill it back in for
; each shape that you need to match

;;;; structure

;;; file

org-file : org-node*

@org-node : headline-node | org-node-basic

@org-node-basic : org-node-basic-element
                | double-blank-line
                | empty-line

empty-line : newline

@org-node-basic-element : drawer
                        ;| blk-dyn
                        | dynamic-block
                        | org-nbe-less-d
                        | paragraph-node ;paragraph-line
                        ; | detached-block-node ; still causing too many issues for well formed blocks

@org-nbe-less-df : greater-block ; block-less-dyn
                 ;| block-end-line
                 ;| babel-call
                 ;| keyword ; lol yep you can affiliate keywords to keywords
                 | keyword-node
                 | comment-element
                 | table-element
                 | plain-list-line

@org-nbe-less-d : @org-nbe-less-df
                | footnote-definition

;;;; everything else

;;; paragraph

; paragraphs can contain every type of object
; ended by empty lines and/or other elements
; this is hard because it is the catch all ... the "not anything else"

; a new element could always start on the next line so if we fall
; through we can only fall through for a single line, then we stitch
; it back together in post

; FIXME do we allow newline? before PARAGRAPH?
; FIXME pretty sure that the @not-newline? after PARAGRAPH-1 never matches anything ?
; we can actually do this now I think since we have successfully defined paragraphs as the negation of the other elements
; FIXME PARAGRAPH-EOF ambiguity
paragraph-node : ( PARAGRAPH
               ;| PARAGRAPH-EOF
               ;| PARAGRAPH-MALFORMED
               | paragraph-line ; this only happens if PARAGRAPH does not match
               | planning-detached ; FIXME isnt this eaten by paragraph-1 ? TODO check
               ; | PARAGRAPH-EOF?
               ) NEWLINE? ; spec says empty lines should associate with the preceeding paragraph

; this only needs to handle the single line cases that are ambiguous for the tokenizer
; note stars is safe here because headlines don't use the standalone stars token
paragraph-line : newline ( LSB
                         | RSB
                         ;| HASH
                         ;| PLUS
                         ;| UNDERSCORE
                         ;| ASTERISK
                         | NEGATED-SET
                         | wsnn
                         | ALPHA
                         | ALPHA-N
                         | DIGIT
                         | DIGIT-N
                         | END-DB
                         | COLON
                         | malformed
                         | stars )+
               | malformed-nl

; FIXME I think we are going to be able to merge nonl and nl once planning is tokenized correctly ?
malformed : planning-dissociated
          | GREATER-BLOCK-MALFORMED
          | UNKNOWN-BLOCK-MALFORMED  ; FIXME move this to the right place XXX variants that start with a newline do not match here

malformed-nl : detached-drawer | detached-block | keyword-malformed
detached-drawer : DRAWER-EOF | DRAWER-PROPS-EOF | DRAWER-MALFORMED ; FIXME distinguish maybe?
; due to changes in the tokenizer the detached blocks always carry their own newline
; however if you are testing from the start of a file you may not see the newline there
; because it is stripped as an imlementation detail
detached-block : GREATER-BLOCK-MALFORMED
               | SRC-BLOCK-EOF | SRC-BLOCK-MALFORMED | UNKNOWN-BLOCK-MALFORMED
keyword-malformed : KEYWORD-ELEMENT-MALFORMED

;;;; *

;;; headlines

;; it is pretty much impossible to parse headlines
;; newline first grammar makes it impossible to parse * H :n: :t:
;; todo-keywords can only be known at runtime * EXPIRED [#A] could be just a title
;; It is possible to write a negated grammar for COMMENT but it pushes enormous
;; complexity into the lexer, ON THE OTHER HAND a second pass parsers is MUCH
;; easier to write a grammar for!

; both planning and property drawer can only have a single newline each before them
;headline-node : headline ( newline ( planning | planning-malformed ) )? property-drawer? ; FIXME property drawers broken again

headline-node : headline ( planning | planning-malformed )? property-drawer?
headline : HEADING

stars : STARS ; ASTERISK+ destorys performance

;;; planning

; XXX this is the grammar that matches the spec
;planning : ( /wsnn* plan-info )+ /wsnn*

; XXX this is the grammar that matches the current elisp org-element
; it is probably preferable to use this one because it makes it possible
; to get a valid parse of a planning line even as someone is typing away
; it is probably worth the additional complexity to make it possible to
; run a parser every single time a new char is typed and still be able
; to get a correct parse for the line, especially since these things
; are the first line after a heading

; FIXME negating timestamps adds massive complexity to the tokenizer
; because you have to account for digits-2 and digits-4 separate from
; digits-n which would be a pain, the spec says that planning lines
; shall only contain repeated planning info elements so I think that
; we could also parse malformed planning lines for consistency but
; not actually do anything with them

; XXX and yet here we are, we need those to parse dates correctly
; anyway, so we can probably manage

;-not-plan-keyword-timestamp-newline : not-newline ; TODO
;not-plan-keyword-timestamp-newline : ( unsyms-less-timestamp | ns-nwt-less-negated | wsnn | word-char-n ) ; not-timestamp-plan-newline
                                   ;| plan-keyword not-colon-newline

planning : PLANNING-ELEMENT
planning-malformed : PLANNING-ELEMENT-MALFORMED
planning-detached : PLANNING-ELEMENT | PLANNING-ELEMENT-MALFORMED

-planning : ( plan-sep plan-info )+ /wsnn* ;not-plan-keyword-timestamp-newline?
plan-sep : /wsnn*
;plan-sep : /wsnn* | not-plan-keyword-timestamp-newline /wsnn

-planning-malformed : plan-mal-info+
plan-mal-info : plan-mal-sep? plan-keyword /COLON plan-mal-sep? ; XXX will gobble?
plan-mal-sep : plan-keyword ; not-colon-newline
             ;| not-plan-keyword-timestamp-newline ; FIXME do it in the lexer

plan-info : plan-keyword /COLON /wsnn* plan-timestamp?
plan-keyword : plan-dead | plan-sched | plan-open | plan-close
plan-timestamp : timestamp
plan-dead : CHARS-DEADLINE
plan-sched : CHARS-SCHEDULED
plan-open : CHARS-OPENED ; XXX suggested improvement
plan-close : CHARS-CLOSED

planning-dissociated : planning

;; comment

;; tags

; XXX the spec is extremely unclear about how tags work
; org-element behavior is currently that you MUST have a non-empty title
; in order for tags to actually be tags the impl for structured
; editing interaction and formatting is inconsistent with this but
; only when the not-actually-tag is separated by a space from the
; priority cookie or TODO keyword I _suspect_ that this is either a
; bug or a conscious design decision, I personally have been annoyed
; by not being able to have tagged nameless headlines, but in a sense
; it does simplify the grammar a bit

;;;; :

;;; drawers

; property drawers are defined by both position and by structure
; due to the bad backtracking behavior in brag it is not possible
; to parse a property drawer correctly if it hits a point where it
; must backtrack because the regular drawer will take precednece
; this is EXTREMELY annoying and there is no way around it
property-drawer : pdrawer-unparsed ;| newline wsnn* properties node-property* /nlws end-drawer
; NOTE when this fails to parse, it WILL parse as a regular drawer
pdrawer-unparsed : DRAWER-PROPS | DRAWER-PROPS-EOF
properties : PROPERTIES-D ; COLON "properties" COLON
;plus : PLUS
;node-property : /newline wsnn* /COLON property-name plus? /COLON [ property-value ] ; the spec does not say it but the implementation allows value to be empty
; the spec for node-property and the implementation are different
; the second wsnn is not documented but is required
; plus is optional but the spec says that empty names are not allowed
; this is false :+: will produce a property whose value is the empty string
; our implementation _will_ break in the case where the elisp one succeeds
; note also that elisp org upcases all property values
;property-name : @not-whitespace* @not-plus-whitespace1
;property-value : wsnn+ not-newline{,1} ; wsnn{1} also valid here since not-newline will matchs wsnn{2,}

; NOTE you cannot nest drawers, an internal drawer heading inside another drawer
; is just text, though it does highlight incorrectly
; XXX the grammer becomes more complicated because regular drawers are allowed to match properties
; this means that we have to fully specify how drawers and property drawers because the ambiguity
; in the grammar is exploited by the parser to increase performance
drawer : DRAWER | DRAWER-EOF | pdrawer-unparsed ; XXX pdrawer-unparsed issues here

;;;; #

;;; comments

comment-element : ( COMMENT-ELEMENT
                    ; | comment-line
                    )+ ; remember kids one-or-more != one-and-maybe-more?
                    NEWLINE?
; comment-line : newline wsnn* HASH ( wsnn+ @not-newline? )?

;;; keywords

@nlwsnn : /newline wsnn*
;babel-call : nlwsnn CALL /COLON not-newline? ; FIXME indentation should NOT be in here it should be higher
; todo-spec-line : TODO-SPEC-LINE

; there is no requirement that there be a space between the key and the value according to org-element
; XXX divergence: in order to make keyword syntax more regular and predicatable we allow the empty keyword
keyword-node : keyword-whole-line NEWLINE?
             ; | /HASH /PLUS kw-key-options? /COLON /wsnn* kw-value? ; somehow the /wsnn* is not matching?
keyword-whole-line : KEYWORD-ELEMENT
;keyword-whole-line : KEYWORD-LINE ; XXX this isn't quite working for some reason but that may be ok
; XXX in order to get consistent behavior the keyword grammar needs to be a subparser
; kw-options here is required to handle cases with whitespace, it needs a post-processing pass
; kw-key-options : @kw-key @keyword-options? ; splice out because we have to reparse

; kw-key : not-whitespace ; XXX there is a tradeoff here between implementation complexity and ambiguity
;kw-value : not-colon-whitespace not-colon-newline* ; ensure that the value does not gobble leading whitespace
; kw-value : not-newline ; but ambiguity ...

; keyword : todo-spec-line | nlwsnn @keyword-line ; FIXME todo-spec-line probably needs to be top level
; keyword : nlwsnn @keyword-line ; FIXME todo-spec-line probably needs to be top level
;keyword-line : ( /HASH /PLUS keyword-key | kw-prefix ) keyword-options? /COLON ( /wsnn* keyword-value )? /wsnn*
;             | /HASH /PLUS keyword-key-sigh ( /wsnn* keyword-value )? /wsnn*

; kw-prefix : AUTHOR | DATE | TITLE | END-DB | BEGIN-DB ; FIXME author date time should just go in not-whitespace probably FIXME END-DB and BEGIN-DB are only keywords if there is no trailing whitespace ?! this is a messy bit
; keyword-options : LSB not-newline? RSB ; FIXME this is almost certainly incorrectly specified

; last colon not followed by whitespace is what we expect here
; XXX NOTE current elisp behavior has ~#+begin:~ as a keyword, I think this is incorrect
;keyword-key : not-sb-colon-whitespace ; XXX paragraph is not set up to handle this, and they need to be keywords
; keyword-key : not-whitespace ; FIXME this should include author date and title surely? ; XXX will match #+k[x]:
; keyword-value : not-colon-newline

; keyword-key-sigh : not-whitespace? ( END-D | PROPERTIES-D )
; keyword-value-sigh : not-colon-newline ; like with paragraph we have to defend against colons down the line
;                   | not-whitespace-l-d? /wsnn* not-newline? COLON not-newline? ; FIXME this seems wrong

;;; affiliated keywords (do not implement as part of the grammar)

; ak-key : CAPTION | HEADER | NAME | PLOT | RESULTS | ak-key-attr
; ak-key-attr : ATTR-PREFIX attr-backend
; attr-backend : @wordhyus

;;; blocks

greater-block : GREATER-BLOCK NEWLINE?

; ugh, matching the arbitrary names here is ...
; not something I think that brag can handle in
; an elegant way because the block-type-name has
; to match ... maybe possible but I don't know how
; to do it or maybe we just don't enforce the
; matching here because we don't actually need to?
; basically, you can't determine the well formedness
; of an org-file from just its syntax because you need
; a stack to keep track of which block you are in

;@block-less-dyn : ( blk-src | blk-unknown ) @empty-line?

dynamic-block : DYNAMIC-BLOCK NEWLINE?

;blk-dyn : /newline blk-dyn-begin blk-dyn-contents newline blk-dyn-end
; XXX elisp impl requires at least wsnn after #+begin: to work
; FIXME NOTE #+begin: asdf is just a keyword if there is no #+end found
;blk-dyn-begin : BEGIN-DB /COLON wsnn blk-line-contents? | BEGIN-DB /COLON ; XXX suggested improvement
; #+begin: without at least whitespace is tably and org-elemently not the start of a dynamic block
; I suppose there might be some completely insane people that use #+begin: without anything following
; it for some other purpose, but really?
;blk-dyn-end : END-DB /COLON
;blk-dyn-contents : org-node-dyn* ;anything except #+end: basically
;org-node-dyn : affiliated-keyword* ( drawer | org-nbe-less-d | paragraph-line | newline )
;org-node-dyn : drawer | org-nbe-less-d | paragraph-line | newline

;@no-headlines : ( PARAGRAPH @not-newline? | line-not-headline)+
; @line-not-headline : newline+ @start-not-headline @not-newline?
                  ; | paragraph ; XXX pretty sure that paragraph should not be needed here and that #+end_ forms should never be parl
                  ;| newline ; XXX not-newline? other elements? XXX cannot have newline by itself it will eat incorrectly
; not clear what order to do this in, parse to the end of the drawer in one go and then
; reparse its contents as org elements? we don't have an intersectional parser here
; using no-headlines it is possible to parse blocks all the way to the end without issue, which
; might be nice ... at least for src and ex blocks, it also makes everything clearer
; the tradeoff for drawers is that you really do have to do a second pass to match org
; behavior, because drawers can contain any other element we might be able to do this
; just by adding other org nodes as contents of the drawer? except other drawers as
; pointed out by the spec ...
; XXX answer: if you allow arbitrary lookahead and manage mode/stack for next-token then you can
; then you can parse drawers in place, technically you only need lookahead if you need to be able
; to know that the drawer is well formed, for drawers this doesn't matter because their contents
; are parsed in the same way, this is not the case for blocks, the alternative (current approach)
; is to parse the strings for the whole drawer in a second pass, but this is bad because the port
; positions are completely out of sync from the original file so more bookkeeping is required
;;;; |

;;; tables

; XXX technically the tokenizer manages the + here
; FIXME newline is not being handled correctly here yet
table-element : ( TABLE-ELEMENT )+ NEWLINE?

;;;;

;;; plain lists

plain-list-line : ( ordered-list-line | descriptive-list-line ) NEWLINE?
ordered-list-line : ORDERED-LIST-LINE
descriptive-list-line : DESCRIPTIVE-LIST-LINE

;;;; [

;;; footnotes

; FIXME there isn't really any such thing as a malformed or eof footnote defintion in the way
; that there can be for blocks or drawers, but because we are reusing some of the internal
; machinery they show up here, there is a TODO to refactor so that this is clear
footnote-definition : FOOTNOTE-DEFINITION NEWLINE?

;;;; <[

timestamp : TIMESTAMP

;;;;

;;; ???????

;; clock
; TODO I think?

;; inline-task
; inline tasks are a dervied concept that abuse a concrete heading
; level and are not something that can be implemented as part of a
; portable grammar, we have no choice for todo keywords, but this
; would require us to parameterize the whole grammar, it could be
; done, but induces enormous amounts of complexity

;; item
; items are a derived concept that nest plain lists and must be
; implemented in the expander

;; section
; sections are derived concepts that nests headings and must be
; implemented in the expander

;;;; tokens

;;; whitespace

;; the elisp org parser parses whitespace as significant
;; XXX suggestion: disallow all leading whitespace or invoke undefined behavior

@newline-or-eof : newline? ; XXXXXXXXXXXXXXXXX this might might might work if no major structure starts with a newline, we will be able to test that once the newline first form is working
nlws : newline ( tab | space )* ; TODO FIXME this has to be parsed as significant whitespace
nlpws : newline+ ( tab | space )* ; variant to handle cases like #+end_src ; TODO FIXME this has to be parsed as significant whitespace
whitespace : newline | space | tab
@wsnn : tab | space ; FIXME need other whitespace chars FIXME need to move multi whitespace to tokenizer

;; this isn't going to work is in ;_;
blank-line : newline /newline ; please tell me that cutting backtracks, that would be beyond wonderful noop
double-blank-line : newline newline /newline

newline : NEWLINE

space : SPACE ; TODO SPACE-N needs to be a lexeme which will cause a branch in here
tab : TAB
; FIXME missing a bunch of other things that count as whitespace
