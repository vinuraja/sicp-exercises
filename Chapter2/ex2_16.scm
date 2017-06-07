; Based on our observations in ex2_14.scm, if the
; the number of arithmetic operations involved
; in an interval arithmetic equation are increased
; then the error bounds become looser.
; So if x => a(b + c) => ab + ac, then we prefer to
; calculate x using the first form, which has 2 operations
; as opposed to form 2 which has 3, to get a tighter
; bound.
;
