#lang sicp
(#%require racket/include)
; Implementation and test have been added in these files.
(include "evaluator.scm")
(include "evaluator_tests.scm")

; It is sufficient to add the clause, since eval will recursively
; call the implementation for evaluation of 'let'.