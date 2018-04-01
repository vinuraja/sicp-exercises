#lang sicp
(#%require racket/include)
(include "constraint.scm")

(define (averager a b c)
  (let ((sum (make-connector))
        (mul (make-connector)))
    (adder a b sum)
    (constant 0.5 mul)
    (multiplier sum mul c)))

(define a (make-connector))
(probe "A" a)
(define b (make-connector))
(probe "B" b)
(define c (make-connector))
(probe "C" c)

(averager a b c)