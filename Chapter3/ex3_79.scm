#lang sicp
(#%require racket/include)
(include "streams.scm")

(define (solve-2nd f y0 dy0 dt)
  (define (y) (integral-delayed (delay (dy)) y0 dt))
  (define (dy) (integral-delayed (delay (ddy)) dy0 dt))
  (define (ddy) (stream-map f (dy) (y)))
  (y))

(stream-ref 
 (solve-2nd (lambda (dy y)
              (+ dy y)) 1 1 0.001) 25)