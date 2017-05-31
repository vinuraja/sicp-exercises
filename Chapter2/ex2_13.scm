; Let's consider the upper bound:
; (c1 + p1*c1) * (c2 + p2*c2)
; => (c1 * c2) * (1 + p1) * (1 + p2)
; => (c1 * c2) * (1 + p1 + p2 + p1*p2)
; now if p1 and p2 are small, p1*p2 will be
; negligible, so the product becomes:
; => (c1 * c2) * (1 + p1 + p2)
;
; Similarly the lower bound:
; (c1 * c2) * (1 - (p1 + p2))
;
; So the formula is: 
; (c1, p1) * (c2, p2) => (c1 * c2, p1 + p2)

