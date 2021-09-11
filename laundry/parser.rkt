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
                        | blk-dyn
                        | org-nbe-less-d
                        | paragraph-node ;paragraph-line
                        ; | detached-block-node ; still causing too many issues for well formed blocks

@org-nbe-less-df : block-less-dyn
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
               | PARAGRAPH-EOF
               | PARAGRAPH-MALFORMED
               | ( PARAGRAPH-2 @not-newline?
                 ; | ( LINK | LINK-AB ) @not-newline?
                 ; | hyperlink @not-newline? ; XXX moved to paragraph parser for consistency, double parsing an issue for now
                 | paragraph-line
                 )+ PARAGRAPH-EOF?
               ) @empty-line? ; spec says empty lines should associate with the preceeding paragraph

; this only needs to handle the single line cases that are ambiguous for the tokenizer
; note stars is safe here because headlines don't use the standalone stars token
paragraph-line : newline (
                          LSB
                         | RSB
                         ;| HASH
                         | PLUS
                         ;| UNDERSCORE
                         | ASTERISK
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
               | parl-tokens-with-newline

; FIXME I think we are going to be able to merge nonl and nl once planning is tokenized correctly ?
malformed : planning-dissociated
          | UNKNOWN-BLOCK-MALFORMED  ; FIXME move this to the right place XXX variants that start with a newline do not match here

@parl-tokens-with-newline : malformed-nl
malformed-nl : detached-drawer | detached-block | keyword-malformed
detached-drawer : DRAWER-EOF | DRAWER-PROPS-EOF | DRAWER-MALFORMED ; FIXME distinguish maybe?
; due to changes in the tokenizer the detached blocks always carry their own newline
; however if you are testing from the start of a file you may not see the newline there
; because it is stripped as an imlementation detail
detached-block : SRC-BLOCK-EOF | SRC-BLOCK-MALFORMED | UNKNOWN-BLOCK-MALFORMED
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
headline-node : headline ( newline ( planning | planning-malformed ) )? property-drawer? ; FIXME property drawers broken again
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

planning : ( plan-sep plan-info )+ /wsnn* ;not-plan-keyword-timestamp-newline?
plan-sep : /wsnn*
;plan-sep : /wsnn* | not-plan-keyword-timestamp-newline /wsnn

planning-malformed : plan-mal-info+
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
                    @empty-line?
; comment-line : newline wsnn* HASH ( wsnn+ @not-newline? )?

;;; keywords

@nlwsnn : /newline wsnn*
;babel-call : nlwsnn CALL /COLON not-newline? ; FIXME indentation should NOT be in here it should be higher
; todo-spec-line : TODO-SPEC-LINE

; there is no requirement that there be a space between the key and the value according to org-element
; XXX divergence: in order to make keyword syntax more regular and predicatable we allow the empty keyword
keyword-node : keyword-whole-line @empty-line?
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

; ugh, matching the arbitrary names here is ...
; not something I think that brag can handle in
; an elegant way because the block-type-name has
; to match ... maybe possible but I don't know how
; to do it or maybe we just don't enforce the
; matching here because we don't actually need to?
; basically, you can't determine the well formedness
; of an org-file from just its syntax because you need
; a stack to keep track of which block you are in

@block-less-dyn : ( blk-src | blk-unknown ) @empty-line?

blk-dyn : /newline blk-dyn-begin blk-dyn-contents newline blk-dyn-end
; XXX elisp impl requires at least wsnn after #+begin: to work
; FIXME NOTE #+begin: asdf is just a keyword if there is no #+end found
blk-dyn-begin : BEGIN-DB /COLON wsnn blk-line-contents? | BEGIN-DB /COLON ; XXX suggested improvement
; #+begin: without at least whitespace is tably and org-elemently not the start of a dynamic block
; I suppose there might be some completely insane people that use #+begin: without anything following
; it for some other purpose, but really?
blk-dyn-end : END-DB /COLON
blk-dyn-contents : org-node-dyn* ;anything except #+end: basically
;org-node-dyn : affiliated-keyword* ( drawer | org-nbe-less-d | paragraph-line | newline )
org-node-dyn : drawer | org-nbe-less-d | paragraph-line | newline

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

@start-not-headline : @not-asterisk-newline1 | stars @not-asterisk-whitespace1 | @word-char-n

; I suspect that we can use complex | simple as a way to try to parse things that can fail
; that we do not what to fall through
blk-line-contents : @not-newline
blk-line-rest : @not-newline

blk-src-malformed : SRC-BLOCK-MALFORMED

blk-src : blk-src-whole ;| /newline blk-src-begin blk-src-contents? nlpws blk-src-end
blk-src-whole : SRC-BLOCK ; XXX requires a nested parser OR chaining the positions of input port in the lexer to produce more than one token i.e. checking in next-token for a list
;blk-src : blk-src-begin newline blk-src-contents? wsnn* blk-src-end
;blk-src-begin : ( BEGIN-SRC wsnn blk-src-line-contents | BEGIN-SRC ) /wsnn*
;blk-src-begin-malformed : BEGIN-SRC /wsnn* newline ; FIXME newline first grammar issues :/
;blk-src-end : END-SRC /wsnn*
;blk-src-contents : no-headlines ; FIXME we just need the bounds for the contents of the block for 2nd pass?
;blk-src-contents : no-headlines-hungry-no-src-end ; this is a version that eats newlines
;blk-src-line-contents : language wsnn blk-src-line-rest-alt | language wsnn*
;language : @not-whitespace ; FIXME

blk-unknown : UNKNOWN-BLOCK

;no-headlines-hungry-no-src-end : line-not-headline-not-src-end+
;line-not-headline-not-src-end : newline* start-not-headline not-newline-not-src-end newline

;;; switches XXX

; still evil but these are a little less evil

; switches and headlines are probably tied for the most aweful parts of the whole grammar

; the drafts of this section of the grammar before I removed the
; special case for -l were probably the inspiration for some of the
; more sanity destorying pages of the Necronomicon

; these should be removed from org-mode ASAP and replaced
; ideally with header arguments which are general and extensible
; before that can happen there needs to be an upgrade path for existing users

;blk-src-line-rest-alt : switches-sane /wsnn blk-src-args-after-switches-sane
                      ;| switches-sane
                      ;| @not-switch @not-newline?

;@--test--switches-sane : switches-sane
;switches-sane : ( switch-sane ( /wsnn format-string )? /wsnn )* switch-sane ( /wsnn format-string )?
;switch-sane : switch-sign alpha
;last-switch-string : wsnn format-string

;@switch-sign : PLUS | HYPHEN

;not-switch : @not-plus-hyphen1
;| PLUS   @not-alpha-newline1
;| HYPHEN @not-alpha-newline1
;| PLUS   @alpha @not-whitespace1
;| HYPHEN @alpha @not-whitespace1

;blk-src-parameters : COLON not-newline ; TODO
; NOTE the correct parameters grammar is a subset of
; the not-a-format-string grammar since it starts with colon
                   ;| wsnn+ COLON not-newline ; TODO

;@blk-src-args-after-switches-sane : blk-src-parameters
;                                  | @not-dq-ph-newline not-newline? ; TODO not-dq-ph-colon-newline for params
;                                  | switch-sign not-alpha-newline1 not-newline?
;                                  | switch-sign ( @alpha @not-whitespace | @word-char-n ) @not-newline?
;                                  | blk-src-args-broken ; XXX user done goofed

; you may have a dq there but then no more dq at all on the line
; this will be extremely weird, but could happen
;blk-src-args-broken : DQ @not-dq-newline?

;format-string : /DQ format-string-contents /DQ
; TODO other escape sequences
;format-string-contents : ( @not-bs-dq-newline | /BS DQ | BS )+

;string : DQ string-contents DQ
; TODO other escape sequences
;string-contents : ( @not-bs-dq | /BS DQ | BS )+

;;;; |

;;; tables

;table : ( table-row | table-row-rule )+
;table : ( /newline /wsnn* ( table-row | table-row-rule ) )+ | table-node

table-element : ( TABLE-ELEMENT )+ ; @empty-line? ; XXX technically the tokenizer manages the + here ; FIXME newline is not being handled correctly here yet

;table-dumb : /newline /PIPE @not-newline ; as a matter of last resort
;table-row : table-cell+ /PIPE?
;table-cell : /PIPE @not-pipe-not-newline? ; this is NOT ambiguous the minimal match is /PIPE+ PIPE? !??!?!
;table-cell : /PIPE /space? ( @not-pipe-bs-newline | /BS PIPE | BS )* /space? ; XXX divergence
;table-row-rule : /PIPE /HYPHEN /not-newline? ; everything else gets wiped

;;;;

;;; plain lists

plain-list-line : ( ordered-list-line | descriptive-list-line ) @empty-line?
ordered-list-line : ORDERED-LIST-LINE
descriptive-list-line : DESCRIPTIVE-LIST-LINE

-plain-list-line : /newline pl-indent ( ordered-list-line | descriptive-list-line )
pl-indent : @wsnn*
-ordered-list-line : bullet-counter plain-list-line-tail?
bullet-counter : ( digits | alphas-unmixed ) ( PERIOD | R-PAREN )
;; XXX NOTE the worg spec is inconsistent with behavior, single letters DO NOT WORK AS COUNTERS
;; TODO ARGH, I've never checked what the org-export backends do O_O AAAAAAAAAAAAAAAAAAAA
;; yet another part of the elisp implementation to deal with FFS
;; html and tex export backends do not support letters in the bullet counter

-descriptive-list-line : bullet-plain plain-list-line-tail?
bullet-plain : HYPHEN | PLUS | wsnn+ ASTERISK 

plain-list-line-tail : @wsnn+ @not-newline | @wsnn+ ; sadly we have to use the dumb parser here due to ambiguity
;plain-list-line-tail : wsnn+ counter-set? check-box? plain-list-tag? not-newline | wsnn
; TODO this can be negated I think
plain-list-yes-we-can : counter-set check-box plain-list-tag not-newline?
                      | counter-set           plain-list-tag not-newline?
                      |             check-box plain-list-tag not-newline?
                      |                       plain-list-tag not-newline?
                      | counter-set check-box pl-not-tag?
                      |             check-box pl-not-tag?
                      |             check-box
                      | counter-set pl-not-check-box?
                      | pl-not-counter-set not-newline? ; technically 

;                     | counter-set check-box plain-list-tag not-newline?

pl-not-counter-set : not-lsb-at-digits-newline
                   | LSB not-at-newline1
                   | LSB AT not-digits-newline
                   | LSB AT digits not-rsb-newline

pl-not-check-box : pl-not-check-box-start not-newline?

pl-not-check-box-start : not-lsb-space-X-hyphen-newline
                       | LSB not-space-X-hyphen-newline1
                       | LSB space-X-hyphen not-rsb-newline1

space-X-hyphen : space | CHAR-UPPER-X | HYPHEN

; XXX spec is unclear and I can't find anything obvious in org-element
; essentially you can't have two colons appear anywhere on the line, I'm not sure whether
; the plain-list-tag can be empty, but since I'm moving to allow empty tags for regularity
; it is consistent to allow empty tags here, also the plain list tag does NOT follow the
; same syntax as headline tags ... it is literally anything until the last paired colons on the line
pl-not-tag : not-colon-newline
           | ( not-colon-newline? COLON )+ not-colon-newline?

counter-set: LSB AT counter-set-content RSB  ; wow this is useful!
counter-set-content : digits
check-box : LSB ( cb-todo | cb-in-process | cb-done ) RSB
cb-todo : space
cb-in-process : HYPHEN ; started ?
cb-done : CHAR-UPPER-X ; AAAAAAAAAAAAAAAAAAAAAAAA ;_; complexity XXX

; TODO check if this taints lowercase as well
plain-list-tag-text : @not-newline
plain-list-tag : pl-tag-end
pl-tag-end : COLON COLON

;;;; [

;;; footnotes

; FIXME there isn't really any such thing as a malformed or eof footnote defintion in the way
; that there can be for blocks or drawers, but because we are reusing some of the internal
; machinery they show up here, there is a TODO to refactor so that this is clear
footnote-definition : FOOTNOTE-DEFINITION @empty-line?
;footnote-definition : ( FOOTNOTE-DEFINITION | FOOTNOTE-DEFINITION-EOF | FOOTNOTE-DEFINITION-MALFORMED)
;footnote-inline : FOOTNOTE-START-INLINE org-node? RSB ; FIXME this is really org-node-less-fd probably

;;;; <[

timestamp : TIMESTAMP

;;;;

;;; ???????

;clock
;inline-task
;item
;section

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

;;; big

bt-asterisk : STARS

bt-colon : END-D | PROPERTIES-D

bt-chars : CHARS-ARCHIVE
         | ARCHIVE ; starts with colon but doesn't end with it to simplify tag parsing, replaced CHARS-ARCHIVE
         | timestamp

bt-chars-plan : CHARS-OPENED
              | CHARS-CLOSED
              | CHARS-DEADLINE
              | CHARS-SCHEDULED

bt-chars-no-title-start : CHARS-COMMENT
                        | RUNTIME-TODO-KEYWORD 

; missing colon plus the other two ???? things that can be followed by whitespace or not a colon?
bt-hash : CALL ; parl ok
        | NAME | HEADER | PLOT | AUTHOR | DATE | TITLE | RESULT | CAPTION | ATTR-PREFIX ; park ok via ak-key
        | BEGIN-DB | END-DB

bt-blk-begin : BEGIN-BLOCK
             | BEGIN-SRC
             | BEGIN-EX

bt-blk-end : END-BLOCK
           | END-SRC ; parl ok via detached-block-node
           | END-EX ; parl ok via detached-block-node

bt-blk : bt-blk-begin | bt-blk-end

bt-lsb : FOOTNOTE-START | FOOTNOTE-START-INLINE

big-tokes-less-d-s-p-cnt-blk : bt-lsb
                             | bt-hash
                             | bt-chars

big-tokes-less-d-s-blk : big-tokes-less-d-s-p-cnt-blk
                       | bt-chars-no-title-start
                       | bt-chars-plan

big-tokes-less-s-blk : big-tokes-less-d-s-blk 
                     | bt-colon

big-tokes-less-s-src-end : big-tokes-less-s-blk
                         | bt-blk-begin
                         | END-BLOCK | END-EX

big-tokes-less-src-end : big-tokes-less-s-src-end
                       | bt-asterisk

big-tokes-less-s-ex-end : big-tokes-less-s-blk
                        | bt-blk-begin
                        | END-BLOCK | END-SRC

big-tokes-less-ex-end : big-tokes-less-s-ex-end
                      | bt-asterisk

big-tokes-less-d-s-p-cnt : big-tokes-less-d-s-p-cnt-blk
                         | bt-blk

big-tokes-less-COMMENT-RTK : big-tokes-less-d-s-p-cnt
                           | bt-asterisk
                           | bt-colon ; spidy sense say that h-rt-title-single is going to need to drop this
                           | bt-chars-plan

big-tokes-less-d-s-p : bt-chars-no-title-start
                     | big-tokes-less-d-s-p-cnt

big-tokes-less-p : bt-asterisk
                 | bt-colon
                 | big-tokes-less-d-s-p

big-tokes-less-d-s : bt-chars-plan
                   | big-tokes-less-d-s-p

big-tokes-less-d : bt-asterisk
                 | big-tokes-less-d-s

big-tokes-less-s : bt-colon
                 | big-tokes-less-d-s

big-tokes : bt-asterisk
          | bt-colon
          | big-tokes-less-d-s

;;; individuals

@alpha : ALPHA | CHAR-UPPER-X ; CHAR-LOWER-L ; XXX complexity
alpha-n : ALPHA-N ; ALPHA-LOWER-N | ALPHA-UPPER-N
digit-n : DIGIT-2 | DIGIT-3 | DIGIT-4 | DIGIT-N ; FIXME in roughly 7979 years will need DIGIT-5
digits : DIGIT | @digit-n ; FIXME +?
alphas : ( alpha | alpha-n )+ ; need the + here due to mixed case
alphas-unmixed : alpha | alpha-n ; needed for cases where case mixing is not allowed
@word-char : DIGIT | alpha
word-char-less-X : DIGIT | ALPHA
word-char-n : @digit-n | @alpha-n ; FIXME rename to alnum-n
wordhyus : ( word-char | HYPHEN | UNDERSCORE | word-char-n )+
wordhy : ( word-char | HYPHEN )+

;;;; negation

;; here be the scary implementation details for what you have to do if
;; you want to be able to parse nearly all of org as a context free
;; language, amazingly it pretty much works, probably because the
;; elisp implementation uses regex for nearly everything and because
;; the set of negated charachters that are required to make it possible
;; to parse org is finite

;;; not yet negated

nwt-less-negated : BS | UNDERSCORE | SQ
                 | LAB | LCB | RCB | L-PAREN
                 | PERCENT

;;; restoration

; because standard EBNF grammars do not support intersection we have
; to explicilty consturct the negated sets we need from the closed
; negated set

; all negated chars must be dealt with explicitly because negation
; must be closed to fit within the grammar every negation that is
; added to the grammar adds significant complexity, but it is orderly
; complexity so in many cases it may be worth it because it allows the
; rest of the grammar to be simpler

non-whitespace-token : @nwt-less-negated | @unsyms
@unsyms :                   AT | BS | DQ | LSB | RSB | LAB | RAB | PIPE | PLUS | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | word-char
@unsyms-less-at :                BS | DQ | LSB | RSB | LAB | RAB | PIPE | PLUS | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | word-char
@unsyms-less-bs :           AT      | DQ | LSB | RSB | LAB | RAB | PIPE | PLUS | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | word-char
@unsyms-less-dq :           AT | BS      | LSB | RSB | LAB | RAB | PIPE | PLUS | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | word-char
@unsyms-less-lsb :          AT | BS | DQ       | RSB | LAB | RAB | PIPE | PLUS | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | word-char
@unsyms-less-rsb :          AT | BS | DQ | LSB       | LAB | RAB | PIPE | PLUS | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | word-char
@unsyms-less-rab :          AT | BS | DQ | LSB | RSB | LAB       | PIPE | PLUS | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | word-char
@unsyms-less-pipe :         AT | BS | DQ | LSB | RSB | LAB | RAB        | PLUS | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | word-char
@unsyms-less-plus :         AT | BS | DQ | LSB | RSB | LAB | RAB | PIPE        | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | word-char
@unsyms-less-hash :         AT | BS | DQ | LSB | RSB | LAB | RAB | PIPE | PLUS        | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | word-char
@unsyms-less-colon :        AT | BS | DQ | LSB | RSB | LAB | RAB | PIPE | PLUS | HASH | PERIOD | R-PAREN         | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | word-char
@unsyms-less-hyphen :       AT | BS | DQ | LSB | RSB | LAB | RAB | PIPE | PLUS | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE          | ASTERISK | word-char
@unsyms-less-asterisk :     AT | BS | DQ | LSB | RSB | LAB | RAB | PIPE | PLUS | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE | HYPHEN            | word-char

@unsyms-less-bs-dq :        AT           | LSB | RSB | LAB | RAB | PIPE | PLUS | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | word-char
@unsyms-less-plus-hyphen :  AT | BS | DQ | LSB | RSB | LAB | RAB | PIPE        | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE          | ASTERISK | word-char
@unsyms-less-bullet :       AT | BS | DQ | LSB | RSB | LAB | RAB | PIPE        | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE                     | word-char
@unsyms-less-pipe-bullet :  AT | BS | DQ | LSB | RSB | LAB | RAB               | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE                     | word-char
@unsyms-less-pipe-bs :      AT      | DQ | LSB | RSB | LAB | RAB        | PLUS | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | word-char
@unsyms-less-alpha :        AT | BS | DQ | LSB | RSB | LAB | RAB | PIPE | PLUS | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | DIGIT
@unsyms-less-sb-colon :     AT | BS | DQ             | LAB | RAB | PIPE | PLUS | HASH | PERIOD | R-PAREN         | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | word-char
@unsyms-less-sb :           AT | BS | DQ             | LAB | RAB | PIPE | PLUS | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | word-char
@unsyms-less-pl-start :     AT | BS | DQ | LSB | RSB | LAB | RAB                      | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE                     ; alpha ; spec says alpha bullets too but impl says nah?
@unsyms-less-prp :          AT | BS | DQ | LSB | RSB | LAB | RAB | PIPE | PLUS | HASH                    | COLON | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | word-char
@unsyms-less-tag :               BS | DQ | LSB | RSB | LAB | RAB | PIPE | PLUS        | PERIOD | R-PAREN                                | HYPHEN | ASTERISK 
@unsyms-less-dq-ph :        AT | BS      | LSB | RSB | LAB | RAB | PIPE          HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE          | ASTERISK | word-char
@unsyms-less-colon-lsb :    AT | BS | DQ       | RSB | LAB | RAB | PIPE | PLUS | HASH | PERIOD | R-PAREN         | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | word-char
@unsyms-less-colon-rsb :    AT | BS | DQ | LSB       | LAB | RAB | PIPE | PLUS | HASH | PERIOD | R-PAREN         | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | word-char
@unsyms-less-lsb-tag :           BS | DQ       | RSB | LAB | RAB | PIPE | PLUS        | PERIOD | R-PAREN                                | HYPHEN | ASTERISK 
@unsyms-less-digit :        AT | BS | DQ | LSB | RSB | LAB | RAB | PIPE | PLUS | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | alpha
@unsyms-less-lsb-at-digit :      BS | DQ       | RSB | LAB | RAB | PIPE | PLUS | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE | HYPHEN | ASTERISK | alpha
@unsyms-less-lsb-X-hyphen : AT | BS | DQ       | RSB | LAB | RAB | PIPE | PLUS | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE          | ASTERISK | word-char-less-X
@unsyms-less-ab-digit-hy :  AT | BS | DQ | LSB | RSB             | PIPE | PLUS | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE          | ASTERISK | alpha
@unsyms-less-timestamp :    AT | BS | DQ                         | PIPE | PLUS | HASH | PERIOD | R-PAREN         | PERCENT | UNDERSCORE          | ASTERISK | alpha
@unsyms-less-hypl-rb-dig :  AT | BS | DQ | LSB       | LAB       | PIPE        | HASH | PERIOD | R-PAREN | COLON | PERCENT | UNDERSCORE          | ASTERISK | alpha

;;; negated sets

not-newline : ( @not-newline1+ | @big-tokes | @word-char-n )+  ; used extensively
not-whitespace : ( @not-whitespace1+ | @big-tokes | @word-char-n )+
not-whitespace-l-d : ( @not-whitespace1+ | @big-tokes-less-d | @word-char-n )+

not-newline-not-asterisk-src-end : ( @not-newline1+ | @big-tokes-less-s-src-end | @word-char-n )+
not-newline-not-src-end : ( @not-newline1+ | @big-tokes-less-src-end | @word-char-n )+

;; individuals
; XXX NOTE make sure you really mean to use these, the not-*-newline1 form is probably what you want
not-newline1 : @not-whitespace1 | space | tab
not-whitespace1 :  NEGATED-SET | @non-whitespace-token

not-at-whitespace1 : ns-nwt-less-negated | unsyms-less-at
not-at-newline1 : not-at-whitespace1 | wsnn
not-at1 : not-at-newline1 | newline

not-lsb-at-digit-newline1 : ns-nwt-less-negated | non-whitespace-token | unsyms-less-digit | wsnn
not-lsb-at-digits-newline : ( @not-lsb-at-digit-newline1 | @big-tokes | alpha-n )+

not-ab-digit-hy-newline1 : ns-nwt-less-negated | unsyms-less-ab-digit-hy | wsnn
not-ab-digits-hy-newline : ( @not-ab-digit-hy-newline1 | @big-tokes-less-p| alpha-n )+

not-lsb-hy-plus-digit-whitespace1 : ns-nwt-less-negated | unsyms-less-hypl-rb-dig | @big-tokes | alpha-n
not-lsb-hy-plus-digit-whitespace : ( @not-lsb-hy-plus-digit-whitespace1 )+

;; unused
; not-plus1 :        ns-nwt-less-negated | @whitespace | unsyms-less-plus
; not-dq1 :          ns-nwt-less-negated | @whitespace | unsyms-less-dq
; not-pipe1 :        ns-nwt-less-negated | @whitespace | unsyms-less-pipe
; not-colon1 :       ns-nwt-less-negated | @whitespace | unsyms-less-colon
; not-pl-start1 :    ns-nwt-less-negated | @whitespace | unsyms-less-pl-start
; not-prp1 :         ns-nwt-less-negated | @whitespace | unsyms-less-prp

not-lsb-space-X-hyphen-newline1 : ns-nwt-less-negated | unsyms-less-lsb-X-hyphen | wsnn
not-lsb-space-X-hyphen-newline : ( not-lsb-space-X-hyphen-newline1 | @big-tokes | word-char-n )+
not-space-X-hyphen-newline1 : not-lsb-space-X-hyphen-newline1 | LSB

ns-nwt-less-negated : NEGATED-SET | @nwt-less-negated

not-plan-keyword-timestamp-newline : ( ns-nwt-less-negated | unsyms-less-timestamp | @big-tokes-less-p | word-char-n )+

not-COMMENT-RTK-lsb-whitespace : ( @not-lsb-whitespace1 | big-tokes-less-COMMENT-RTK | word-char-n )+

not-COMMENT-RTK-lsb-tag-whitespace : ( @not-lsb-tag-whitespace1 | big-tokes-less-COMMENT-RTK | word-char-n )+
not-lsb-tag-whitespace1 : ns-nwt-less-negated | unsyms-less-lsb-tag

not-alpha-whitespace1 : ns-nwt-less-negated | unsyms-less-alpha
not-alpha-newline1 : @not-alpha-whitespace1 | wsnn

not-digit-newline1 : ns-nwt-less-negated | unsyms-less-digit | wsnn
not-digits-newline : ( @not-digit-newline1 | @big-tokes | alpha-n )+

not-asterisk-whitespace1 :    ns-nwt-less-negated | unsyms-less-asterisk
not-asterisk-newline1 :       @not-asterisk-whitespace1 | wsnn
not-asterisk1 :               not-asterisk-newline1 | newline

not-lsb-whitespace1 : ns-nwt-less-negated | unsyms-less-lsb
not-lsb-newline1 : @not-lsb-whitespace1 | wsnn

not-rsb-newline1 : ns-nwt-less-negated | wsnn | unsyms-less-rsb
not-rsb-newline : ( @not-rsb-newline1 | @big-tokes | word-char-n )+

not-colon-lsb-whitespace1 : ns-nwt-less-negated | unsyms-less-colon-lsb
not-colon-lsb-whitespace : ( not-colon-lsb-whitespace1 | @big-tokes-less-d | word-char-n )+
not-colon-lsb-newline1 : not-colon-lsb-whitespace1 | wsnn

not-colon-rsb-newline1 : ns-nwt-less-negated | wsnn | unsyms-less-colon-rsb
not-colon-rsb-newline : ( not-colon-rsb-newline1 | @big-tokes-less-d | word-char-n )+

not-bullet-pipe1 : ns-nwt-less-negated | @whitespace | unsyms-less-pipe-bullet
not-plus-hyphen1 : ns-nwt-less-negated | @whitespace | unsyms-less-plus-hyphen

not-pl-start-newline1 : ns-nwt-less-negated | wsnn | unsyms-less-pl-start
not-prp-newline1 :      ns-nwt-less-negated | wsnn | unsyms-less-prp

not-colon-whitespace1 : ns-nwt-less-negated | unsyms-less-colon
not-colon-whitespace : ( @not-colon-whitespace1 | @big-tokes-less-d | word-char-n )+
not-colon-newline1 :  not-colon-whitespace1 | wsnn
not-colon-newline :    ( @not-colon-newline1 | @big-tokes-less-d | word-char-n )+

not-pipe-not-newline1 : ns-nwt-less-negated | wsnn | unsyms-less-pipe
not-pipe-not-newline : ( @not-pipe-not-newline1 | @big-tokes | word-char-n )+

not-pipe-bs-newline1 : ns-nwt-less-negated | unsyms-less-pipe-bs | wsnn
not-pipe-bs-newline : ( @not-pipe-bs-newline1 | @big-tokes | word-char-n )+

not-tag-whitespace1 : ns-nwt-less-negated | unsyms-less-tag
not-tag-whitespace : @not-tag-whitespace1+ ; TODO which big tokes?
not-tag-newline1 : not-tag-whitespace1 | wsnn
not-tag-newline : @not-tag-newline1+ ; TODO which big tokes?

not-pl-start-whitespace1 : ns-nwt-less-negated | unsyms-less-pl-start
not-plus-whitespace1 :     ns-nwt-less-negated | unsyms-less-plus
not-plus-newline1 : not-plus-whitespace1 | wsnn
not-bs-dq-whitespace1 :    ns-nwt-less-negated | @unsyms-less-bs-dq
not-bs-dq-newline1 : space | tab | @not-bs-dq-whitespace1
not-bs-dq1 : whitespace | @not-bs-dq-whitespace1
not-bs-dq-newline : ( @not-bs-dq-newline1 | word-char-n )+
not-bs-dq : ( @not-bs-dq1 | word-char-n )+

not-dq-newline1 : ns-nwt-less-negated | @unsyms-less-dq | wsnn
not-dq-newline : ( @not-dq-newline1 | big-tokes | word-char-n )+

not-dq-ph-newline1 : ns-nwt-less-negated | @unsyms-less-dq-ph | wsnn
not-dq-ph-newline : ( @not-dq-ph-newline1 | big-tokes | word-char-n )+

not-hash-newline1 : ns-nwt-less-negated | @unsyms-less-hash | wsnn

not-pipe-bullet-not-whitespace1 : ns-nwt-less-negated | unsyms-less-pipe-bullet
;not-pipe-bullet-not-newline1 : space | tab | not-pipe-bullet-not-whitespace1 ; unused
;not-pipe-bullet-not-newline : ( @not-pipe-bullet-not-newline1+ | @big-tokes )+ ; unused
not-sb-colon-whitespace1 : ns-nwt-less-negated | unsyms-less-sb-colon
not-sb-colon-whitespace : @not-sb-colon-whitespace1+
not-sb-whitespace1 : ns-nwt-less-negated | unsyms-less-sb
not-sb-whitespace : ( @not-sb-whitespace1 | @big-tokes | @word-char-n )+
