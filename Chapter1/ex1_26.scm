; In the correct case the recurrence relation for runtime
; can be written as:
; T(n) = T(n/2) + c, so T(n) = O(log(n)),
; while in the case of the doubly written exponential, we have:
; T(n) = 2 * T(n/2) + c, so T(n) = 2 * (2 * T(n/4) + c) + c,
; which makes T(n) = O(n).

