; This code works by recursively calling partial-tree on the left
; half of n, then the right half (leaving element n/2 + 1), and
; then constructing the tree using the left half, element n/2 + 1
; and the right half.

;         5
;       /   \
;      1      9
;       \    / \
;        3  7  11

; Order of growth would be T(n) = 2*T(n/2) + O(1), and depth
; of tree would be O(log(n)), so overall should be O(nlog(n)).
