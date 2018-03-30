#lang sicp
; If initialization isn't done that way, the state of the output
; signal may not be as expected. For example, if we add an inverter
; to a signal that is 0, and if we don't run the action immediately,
; the output won't become 1 even during propagation. This is because
; there is no propagation if the signal doesn't change in value.

; The above explanation applies to the half-adder being added to wires
; where A is 0 and B is 1. Without immediate action, the S won't be 1,
; because when propagate is run, no signal has changed value.