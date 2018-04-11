#lang sicp
(#%require racket/include)
(include "streams.scm")

; The computed stream is basically the decimal (if using radix as 10)
; form of the fraction formed by num / den.

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) 
           den 
           radix)))

(define a (expand 1 7 10))

(define b (expand 3 8 10))
