; Space complexity: O(amount)
; Runtime complexity: O(amount^denominations)

; count-change time order of growth:

; 1. cc(n, 1) = O(n)
; 2. cc(n, 2) = cc(n, 1) + cc(n-5, 2)
; 3. each 2. step is O(n) and there are roughly n/5 such steps
; so we have O(n^2)
; by analogue we get O(n^k) (k currencies) for cc(n, k)

