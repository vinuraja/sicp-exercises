#lang sicp
(#%require racket/include)
(include "lazy_evaluator.scm")
(include "lazy_evaluator_tests.scm")

(eval '(define count 0) genv)
(eval '(define (id x) (set! count (+ count 1)) x) genv)
(eval '(define w (id (id 10))) genv)

; The value of count is 1, because only the outermost id is evaluated,
; the innermost is passed in as a delayed-arg, and stored as a thunk
; and never evaluated because it isn't forced.
(check-equal? (eval 'count genv) 1)

; w returns a thunk of (id 10)
(check-true (thunk? (eval 'w genv)))

; count remains as 1 as we haven't forced the previous thunk yet.
(check-equal? (eval 'count genv) 1)