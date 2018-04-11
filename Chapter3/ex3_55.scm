#lang sicp
(#%require racket/include)
(include "streams.scm")

(define (partial-sums S)
  (cons-stream (stream-car S) (add-streams (stream-cdr S) (partial-sums S))))

(define partial-sums-integers
  (partial-sums integers))