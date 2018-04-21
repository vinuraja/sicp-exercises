#lang sicp
(#%require racket/include)
(include "streams.scm")

(define (solve f y0 dt)
  (define (y) (integral-delayed (delay (dy)) y0 dt))
  (define (dy) (stream-map f (y)))
  (y))

(stream-ref 
 (solve (lambda (y) y) 1 0.001) 1000)