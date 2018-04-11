#lang sicp
(#%require racket/include)
(include "streams.scm")

(define (integrate-series S)
  (mul-streams (stream-map / ones integers) S))

; 1 + 1/2 + 1/3 + ...
(define i (integrate-series ones))

(define exp-series
  (cons-stream 
   1 (integrate-series exp-series)))

(define cosine-series 
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))