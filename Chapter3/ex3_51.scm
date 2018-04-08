#lang sicp
(#%require racket/include)
(include "streams.scm")

(define (show x)
  (display x)
  (newline)
  x)

(define x 
  (stream-map 
   show 
   (stream-enumerate-interval 0 10)))