#lang sicp
; Ans 1:
; 3! or 6 orders for Peter (+$10), Paul (-$20), Mary (-1/2)
; and $100 starting.
;
; Pe, Pa, Ma : $45
; Pe, Ma, Pa : $35
; Pa, Pe, Ma : $45
; Pa, Ma, Pe : $50
; Ma, Pa, Pe : $40
; Ma, Pe, Pa : $40
;
; Ans 2:
; One interleaved issue: if everyone reads the balance as $100,
; and does their calculations, and the last one is Mary, the
; balance would be $50. If Paul, $80 and if Peter, $90.