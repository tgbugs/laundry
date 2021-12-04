#lang brag

; TODO clean this up as a sub-grammar

planning : ( plan-sep plan-info )+ /wsnn* ;not-plan-keyword-timestamp-newline?
plan-sep : /wsnn*

plan-info : plan-keyword /COLON /wsnn* plan-timestamp?
plan-keyword : plan-dead | plan-sched | plan-open | plan-close
plan-timestamp : timestamp
plan-dead : CHARS-DEADLINE
plan-sched : CHARS-SCHEDULED
plan-open : CHARS-OPENED ; XXX suggested improvement
plan-close : CHARS-CLOSED

timestamp : TIMESTAMP
