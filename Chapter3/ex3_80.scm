#lang sicp
(#%require racket/include)
(include "streams.scm")

(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define (dvC) (scale-stream (iL) (/ -1 C)))
    (define (vC) (integral-delayed (delay (dvC)) vC0 dt))
    (define (diL) (add-streams
                   (scale-stream (vC) (/ 1 L))
                   (scale-stream (iL) (/ (* -1 R) L))))
    (define (iL) (integral-delayed (delay (diL)) iL0 dt))
    (cons (vC) (iL))))

(define RLC0 ((RLC 1 1 0.2 0.1) 10 0))