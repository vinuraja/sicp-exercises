#lang sicp
; Defining map as a procedure in the implemented language works,
; while using it as a primitive doesn't because map takes a
; closure as an input, and then the closure requires evaluation
; within the implemented language (including access to the
; environment) to work. For example, we could change the syntax of
; the implemented language to say (closure (x) (@ x y)) for
; (lambda (x) (/ x y)), but this needs to be evaluated as the
; implemented language for this to work. If we pass this directly
; to the primitive map implementation within the implementation language,
; no evaluation will happen, and it fails.