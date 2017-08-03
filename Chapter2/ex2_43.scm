; Because of this error the (queen-cols (- k 1)) is evaluated
; n times, and this is true recursively, so the total runtime
; becomes T * n^2.

