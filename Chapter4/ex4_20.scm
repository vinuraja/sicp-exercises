#lang sicp
(#%require racket/include)
(include "evaluator.scm")
(include "evaluator_tests.scm")

; Using 'letrec', (f 5) will have both even? and odd? in it's current frame;
; initially unassigned, and then set! to the appropriate expressions while
; evaluating (f 5) and reaching <rest of body>. The lambda expressions
; themselves will use the values of even? and odd? within this frame, because
; the procedures are created within the same frame.
; Using 'let', (f 5) will try to bind even? and odd? to their appropriate
; expressions immediately, but the problem is that the lambda expressions
; themselves are converted to procedures whose environemnt doesn't contain
; variables named even? nor odd?. Hence trying to run even? and odd? will
; fail!
