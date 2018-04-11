
; With memoization, the number of additions required for
; calculating nth fibs is n, but with no memoization this
; becomes exponentially greater. To understand the latter,
; we can construct a tree with each node representing 1
; addition for the kth fibonacci number and it's children
; representing additions for the k-1th and k-2th fibonacci
; number calculations. So the total number of additions will
; be equal to the number of nodes in the tree, which should be
; O(2^n), which is exponentially greater than n.