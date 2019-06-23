#lang sicp

; The problem is with duplication of the variable that the
; accumulation-function is interested in. For example, if Ben
; wants to sum up the salaries of all the 'wheels' in the company,
; he might naively do this:
; (sum ?amount
;      (and (wheel ?person)
;           (salary ?person ?amount))
; which will add (Warbucks Oliver)'s salary 4 times, instead of just once.

; A possible fix is to dedup the frames, by taking only those frames whose
; bindings don't match bindings from any previous frame.