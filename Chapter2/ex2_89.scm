#lang sicp
(define (first-term term-list)
  (make-term (- (length term-list) 1) (car term-list)))

(define (adjoin-term term term-list)
  (if (= (order term) (length term-list)) (car (coeff term) term-list)
  (adjoin-term term (car 0 term-list))))
