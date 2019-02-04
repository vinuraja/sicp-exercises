;#lang sicp
#lang lazy
(define (unless condition 
                usual-value 
                exceptional-value)
  (if condition 
      exceptional-value 
      usual-value))

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

(factorial 5)

; (factorial 5) in an applicative-order language will infinitely recurse,
; because the termination condition won't actually terminate anything. The
; factorial method will continue to be called.

; (factorial 5) in a normal-order language will give the right answer without
; infinite recursion because the termination condition will prevent further
; execution of the factorial method.