#lang sicp

; We can do this by having the lower-level methods of
; add-complex, sub-complex, etc. as well as sine, cosine,
; use apply-generic so that they can be done for both
; ordinary numbers as well as rational numbers.