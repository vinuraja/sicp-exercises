#lang sicp
; This is added to amb_evaluator_tests.scm

(let ((pairs '()))
  (if-fail 
   (let ((p (prime-sum-pair 
             '(1 3 5 8) 
             '(20 35 110))))
     (permanent-set! pairs 
                     (cons p pairs))
     (amb))
   pairs))

; Will return '((8 35) (3 110) (3 20)). The pairs are in opposite
; order because the permanent-set! line adds the new pair to the
; beginning.