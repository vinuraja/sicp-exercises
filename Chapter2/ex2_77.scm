; The method apply-generic depends on their being an operation
; for the particular type-tag. In this case, there is no method
; 'magnitude' for type-tag 'complex' unless we add it.

; magnitude -> apply-generic, type-tags -> get, repeated 2 times, for complex
; and for rectangular/polar.

; Procedure magnitude.
