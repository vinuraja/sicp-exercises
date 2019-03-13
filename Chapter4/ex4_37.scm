#lang sicp

(#%require (only sicp-pict amb))

(define (require p)
  (if (not p) (amb)))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

; Compared to the algorithm in ex4_35.scm, this is more efficient,
; because it is an O(n^2) algorithm, because it has loops only
; for i and j; k is derived directly from i and j. Whereas the
; ex4_35.scm algorithm is O(n^3) because it has loops for i, j and k.
; Also, this algorithm, prunes away some of the search-space by
; using (require (>= hsq ksq)), which reduces n to around high/(sqrt 2).
; making it even more efficient.