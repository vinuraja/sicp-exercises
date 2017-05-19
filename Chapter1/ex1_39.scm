(define (cont-frac n d k)
  (cont-frac-iter n d k 0))

(define (cont-frac-iter n d k frac)
  (if (= k 0)
      frac
      (cont-frac-iter n d (- k 1) (/ (n k) (+ (d k) frac)))))

(define (tan-cf x k)
  (cont-frac (lambda (i)
                  (if (= i 1)
                      x
                      (- (* x x))))
             (lambda (i) (- (* i 2) 1))
             k))

