#lang sicp
(#%require racket/include)
; Implementation and test have been added here.
(include "evaluator.scm")
(include "evaluator_tests.scm")

; Alternatively, 'and' and 'or' could be implemented on top of 'cond'.
; For 'and', the not of each and-exp would be a condition in cond, and for
; 'or, each or-exp would be a condition. The return values will need to
; be setup appropriately.