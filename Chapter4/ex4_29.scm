#lang sicp
(#%require racket/include)
(include "lazy_evaluator.scm")
(include "lazy_evaluator_tests.scm")

(eval '(define (fact n)
         (if (= n 1)
             1
             (* n (fact (- n 1))))) genv)

; This takes a longer time without memoization because
; n is (fact 5), and it has to be calculated again and
; again 3 times (3 uses of n), and similar repetitions
; are done 5! or 120 times.
(time (eval '(fact (fact 5)) genv))

(eval '(define count 0) genv)
(eval '(define (id x) (set! count (+ count 1)) x) genv)
(eval '(define (square x) (* x x)) genv)

; Toggle memoization vs no memoization along with toggling
; force-it in lazy_evaluator.scm.

; memoization
(check-equal? (eval '(square (id 10)) genv) 100)
(check-equal? (eval 'count genv) 1)

; no memoization
;(check-equal? (eval '(square (id 10)) genv) 100)
;(check-equal? (eval 'count genv) 2)