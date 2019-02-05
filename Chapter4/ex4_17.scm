#lang sicp
; In the scanned program there is an extra frame due to
; the 'let' we add, containing the defines as definitions,
; which is missing when just sequentially evaluating without
; a transformation.
; In either case, when evaluating (e3) we can lookup the right
; definitions for 'u' and 'v', so it doesn't make a difference
; whether or not we use the transformation.
; For interpreting without the extra frame, we can change
; 'make-procedure' to also store the definitions as bound variables
; and values, and transform the body to remove the definitions.
; Then for 'apply', we extend the environment with these bounded
; variables and values, if available.