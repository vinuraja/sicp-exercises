#lang sicp
(#%require racket/include)
(include "evaluator.scm")
(include "evaluator_tests.scm")

; I don't have a strong reason (yet) about placing scan-out-defines
; in either procedure-body or make-procedure.