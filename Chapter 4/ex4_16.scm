#lang sicp
(#%require racket/include)
(include "evaluator.scm")
(include "evaluator_tests.scm")

; Keeping scan-out-defines in make-procedure, as opposed to procedure-body
; would be more efficient since make-procedure is just called once during
; evaluation; at time of the definition of the lambda, whereas procedure-body
; is called each time the lambda is applied.
