;#lang sicp
(#%require (only racket/base make-hash hash-set! hash-ref))

(define h (make-hash))
(define (put op type item)
  (hash-set! h (list op type) item))
(define (get op type)
  (hash-ref h (list op type) false))