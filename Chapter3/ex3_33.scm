#lang sicp
(#%require racket/include)
(include "constraint.scm")

(define (averager a b c)
  (let ((sum (make-connector))
        (mul (make-connector)))
    (adder a b sum)
    (constant mul 0.5)
    (multiplier mul c)))