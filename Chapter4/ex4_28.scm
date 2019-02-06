#lang sicp
(#%require racket/include)
(include "lazy_evaluator.scm")
;(include "lazy_evaluator_tests.scm")

(#%require rackunit)
(eval '(define (f pred v)
         (if (pred v)
             1
             0)) genv)
; This will fail if actual-value is changed to eval in evaluator-apply
; because the arg pred is taken as a delayed arg, and will be a thunk
; instead of a procedure, and needs to be forced using actual-value.
(check-equal? (eval '(f (lambda (x) (= x 0)) 2) genv) 0)