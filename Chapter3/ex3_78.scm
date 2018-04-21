#lang sicp
(#%require racket/include)
(include "streams.scm")

(define (solve-2nd a b dt y0 dy0)
  (define (y) (integral-delayed (delay (dy)) y0 dt))
  (define (dy) (integral-delayed (delay (ddy)) dy0 dt))
  (define (ddy) (add-streams (scale-stream (y) b)
                             (scale-stream (dy) a)))
  (y))

(stream-ref 
 (solve-2nd 1 1 0.001 1 1) 25)