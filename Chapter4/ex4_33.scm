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
         (lambda (m) (m x y))) genv)
(eval '(define (car z)
         (z (lambda (p q) p))) genv)
(eval '(define (cdr z)
         (z (lambda (p q) q))) genv)

; It fails because the list '(a b c) is a list created using
; the implementation language, using its own cons, and not
; the cons that we defined above, within the implemented language.
(check-equal? (force-it (eval '(car (cons 'a 'b)) genv)) 'a)
(check-equal? (force-it (eval '(car '(a b c)) genv)) 'a)
(check-equal? (force-it (eval '(car '(a b c)) genv)) 'a)