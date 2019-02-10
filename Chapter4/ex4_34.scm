#lang sicp
(#%require racket/include)
(#%require rackunit)
(#%require (only racket/base exn:fail?))
(include "lazy_evaluator.scm")
(include "lazy_evaluator_tests.scm")

; The cons-lambda is required to special-case the returned
; lambda, so that we can print the lists appropriately from
; the driver-loop.
(eval '(define (cons x y)
         (cons-lambda (m) (m x y))) genv)
(eval '(define (car z)
         (z (lambda (p q) p))) genv)
(eval '(define (cdr z)
         (z (lambda (p q) q))) genv)

; Some validation tests, to see if the above definitions are working.
(check-equal? (force-it (eval '(car (cons 'a 'b)) genv)) 'a)
(check-equal? (force-it (eval '(car '(a b c)) genv)) 'a)
(check-equal? (force-it (eval '(car '(a b c)) genv)) 'a)

(driver-loop)
; Driver-loop should give these outputs for the inputs.

; '(a b c)
; (a b c)

; '()
; ()

; '(a)
; (a)

; '(a b c d e)
; (a b c d e)

; '(a b c d e f)
; (a b c d e ...)