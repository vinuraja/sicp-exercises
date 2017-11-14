; 1. The procedures produce the same result. This is basically
; in-order printing of trees.

; 2. They both have different orders of growth. The first one has
; an append operation at every node, so the growth would be something
; like O(n/2) + O(n/4 * 2) + ... log(n) = O(nlog(n))
; For the second one, there is only a 'cons' per node needed, we just
; pass the result-list around other than that. If in scheme, we use
; pass by references for lists, then that's O(n), otherwise it's
; O(nlog(n)) again. I assume we can pass lists around by reference, because
; lists are immutable.
