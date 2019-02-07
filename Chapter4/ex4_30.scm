#lang sicp
(#%require racket/include)
(include "lazy_evaluator.scm")
(include "lazy_evaluator_tests.scm")

(eval '(define (for-each proc items)
         (if (null? items)
             'done
             (begin (proc (car items))
                    (for-each proc 
                              (cdr items))))) genv)

(eval '(for-each
        (lambda (x) (newline) (display x))
        (list 57 321 88)) genv)

; 1. In the case of this example things work because newline and display are
; primitive procedures are not thunked.
