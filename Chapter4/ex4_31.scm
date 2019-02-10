#lang sicp
(#%require racket/include)
(include "evaluator.scm")
(include "evaluator_tests.scm")

(eval '(define (fact-memo (n lazy-memo))
         (if (= n 1)
             1
             (* n (fact-memo (- n 1))))) genv)

(eval '(define (fact (n lazy))
         (if (= n 1)
             1
             (* n (fact (- n 1))))) genv)

(time (eval '(fact-memo (fact-memo 5)) genv))
; This takes a longer time without memoization because
; n is (fact 5), and it has to be calculated again and
; again 3 times (3 uses of n), and similar repetitions
; are done 5! or 120 times.
(time (eval '(fact (fact 5)) genv))