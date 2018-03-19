#lang sicp
(define (make-accumulator sum)
  (lambda (value)
    (set! sum (+ sum value))
    sum))