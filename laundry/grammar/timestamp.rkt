#lang brag
;;; timestamps

; the org spec is currently silent on how org will handle dates beyond
; 0000 and 9999, the relevant standards for expanded representations
; are ISO 8601-1:2019,5.2.2.3 and ISO 8601-2:2019,8.4.3. The problem
; with expanded representations is that they require all parties to
; agree ahead of time how wide the year is and prefix with + and
; leading zeros, I am not sure that this is as robust as we would like
; 0000-01-01 is january first of the year zero, -0001-01-01 is jan
; first of the year 1BCE, and +10000-01-01 for jan 1 of 10000CE 5
; digits must be prefixed by the plus sign, the standard is not
; entirely clear, but I think that we could do PLUS digits HYPHEN
; or DIGIT-4 HYPHEN and everything would work out

; on a separate but related note, org timestamps are always in local
; time and do not currently support timezones, which is a problem

; further absurdities that are related to the timezone issue: the
; date/time implementation as a text format completely fails if
; authoring on another planet

; given my objective to ensure that org documents can be interpreted
; without having to stick stupid things like #+planet: mars in the
; header or risk your earthling readers getting incorrect dates --- I
; suggest that org switch to storing all dates and times in earth zulu
; time or with an offset as it stands, the timezone setting of the
; computer drafting the document must be known to determine what day
; it was etc. users could still draft using <2020-12-16> but C-c C-c
; would add the correct offset? actually this is tricky, because there
; are two cases, one where the location is clear, "napoleon on [1812-01-01]"
; and the other where it is not, the issue is that a single date refers
; to as many different time intervals as there are timezones and without
; knowing the timezone you don't know the date, similar issue if we were
; to try to convert to sols
; https://www.eecis.udel.edu/~mills/missions.html
; https://www.eecis.udel.edu/~mills/ipin.html
; https://en.wikipedia.org/wiki/Timekeeping_on_Mars
; HOWEVER having applied a bit more brain power to the problem, let's
; just assume that clock synchonization will happen between earth and
; mars so that the unix epoch could be synchronized between the frames
; probably with multiple reference stations in free fall and/or in deep
; space to account for gravitaitonal differences etc. so actually the
; only issue would be a practical notational one, or a UI one, since we
; can use timestamp with timezone or just straight epoc for this to work
; earth vs martian year/day is a much harder issue

; ([+-][0-9]\+|[0-9]{4})(-[0-9]{2}){2}
; ([0-9]{2}:[0-9]{2}(:[0-9]{2}(,[0-9]{1,9})?)?[+-][0-9]{2}:[0-9]{2})?
; followed by an optional day of the week and then the repeat or delay for example
; [+10000-01-01 10:11:00,992315771-04:00 Sat]
; or more temporally local
; [2021-03-03 17:43-07:00 Sat]

timestamp : ts-diary | ts-active | ts-inactive | ts-range-active | ts-range-inactive
ts-diary : LAB "%%" L-PAREN ts-diary-sexp R-PAREN RAB ; this spec doesn't make any sense
;  SEXP can contain any character excepted > and \n.
ts-diary-sexp : NOT-RAB-NOT-NEWLINE+
ts-active : /LAB date ( /space+ time )? ts-rod? /RAB
ts-inactive : /LSB date ( /space+ time )? ts-rod? /RSB
ts-range-active : ts-active /hh ts-active | /LAB date /space+ time /HYPHEN time ts-rod /RAB ; wrong due to ts-active including ts-rod
ts-range-inactive : ts-inactive /hh ts-inactive | /LSB date /space+ time /HYPHEN time ts-rod /RSB ; wrong due to ts-inactive including ts-rod
hh : HYPHEN HYPHEN

;; better to handle date correctness outside the parser to reduce token complexity
;; date
date : @date-normal | @date-ex
@date-suffix : /HYPHEN month /HYPHEN day ( space+ day-abbrev )?
date-normal : year date-suffix
year : DIGIT-4
month : DIGIT-2
day : DIGIT-2
date-ex : date-sign year-ex date-suffix
year-ex : digits ; XXX pretty sure this is abuse of notation for iso8601 but whatever
date-sign : PLUS | HYPHEN

;; time
time : hour /COLON minute
hour : DIGIT-2
minute : DIGIT-2
day-abbrev : not-lsb-hy-plus-digit-whitespace ; FIXME non whitespace not + - ] > DIGIT \n

;; repeat or delay
ts-rod : ts-rod-entry ts-rod-entry?  ; FIXME one repeater and one warning delay? not sure what this is
ts-rod-entry : ts-rod-mark ts-rod-value ts-rod-unit
ts-rod-mark : "+" | "++" | ".+" | "-" | "--"
ts-rod-value : digits
ts-rod-unit : "h" | "d" | "w" | "m" | "y"

space : SPACE
digits : DIGITS
not-lsb-hy-plus-digit-whitespace : WAT
