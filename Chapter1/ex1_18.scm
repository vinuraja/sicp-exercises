(define (mul-iter a b p)
  (cond ((= b 0) p)
        ((even? b) (mul-iter (* a 2) (/ b 2) p))
        (else (mul-iter a (- b 1) (+ a p)))))
    
(define (fast-mul a b)
  (mul-iter a b 0))
