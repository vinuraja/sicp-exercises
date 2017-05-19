(define (cont-frac n d k)
  (cont-frac-iter n d k 0))

(define (cont-frac-iter n d k frac)
  (if (= k 0)
      frac
      (cont-frac-iter n d (- k 1) (/ (n k) (+ (d k) frac)))))

(define (e k)
  (+ (cont-frac (lambda (i) 1.0)
                  (lambda (i)
                    (if (= 2 (remainder i 3))
                        (* 2 (+ (/ i 3) 1))
			1))
                  k)
      2))

; (e 12) gives 2.7947716615434857, whereas e is actually 2.7182818284590452.

