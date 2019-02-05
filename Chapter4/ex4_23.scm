#lang sicp
; In the case of a single expression, the text version would
; just directly return the analyzed expression, whereas Alyssa's
; version would have to run execute-sequence to figure out there's
; only 1 proc and then apply the env to it.
; When there are 2 expressions, the text version would directly
; call both the analyzed expressions in sequence, whereas Alyssa's
; version would have to run execute-sequence to figure out there
; are 2 procs and then apply the env to them in sequence.