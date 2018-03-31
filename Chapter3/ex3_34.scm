#lang sicp
; The constraints will work in one direction, from a -> b,
; but if the value of b changes, multiplier can't change
; the value of a. This is because multiplier needs 2 values
; to change the 3rd one, and it doesn't know that both it's
; multiplicands are the same.