#lang sicp
(#%require racket/include)
; Implemented and tested in these files.
(include "evaluator.scm")
(include "evaluator_tests.scm")

; The implementation is closely based on this example of how to do
; recursion in Scheme without the use of 'define'.

;(((lambda (x) (x x))
;  (lambda (fact-gen-rec)
;    (let ((fact-gen (lambda (n)
;                       ((fact-gen-rec fact-gen-rec) n))))
;      (lambda (n)
;        (if (= n 0)
;            1
;            (* n (fact-gen (- n 1))))))))
; 5)

; The implementation basically does the same as the below transformation of
; the example show in ex4.8. We have a recursive definition and a let, which
; transforms it into the original function name used in the body, so that we
; don't have to make any changes to the body. Since we use let, and not define,
; we don't pollute the global environment with the 'rec-fn' name that we use. :)

;(define (fib n)
;  (let ((rec-fn
;         (lambda (fib-iter-rec a b count)
;           (let ((fib-iter (lambda (a b count)
;                             (rec-fn rec-fn a b count))))
;             (if (= count 0)
;                 b
;                 (fib-iter (+ a b)
;                           a 
;                           (- count 1)))))))
;    (rec-fn rec-fn 1 0 n)))