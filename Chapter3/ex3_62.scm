#lang sicp
(#%require racket/include)
(include "streams.scm")

(define (invert-series S)
  (if (= (stream-car S) 0)
      (error "Cannot invert series! Stream starting with 0 constant term.")
      (scale-stream (invert-unit-series (scale-stream S (/ 1 (stream-car S))))
                    (/ 1 stream-car S))))

(define (div-series S1 S2)
  (mul-series S1 (invert-series S2)))