(define (p) (p))

(define (test x y) 
  (if (= x 0) 
      0 
      y))
; In applicative order this will cause an infinite recursion because of (p).
; In normal order this would return 0 because the if-clause will evaluate to 0.
