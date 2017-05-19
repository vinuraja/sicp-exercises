(define (cont-frac-rec n d k i)
  (if (> i k)
      0
      (/ (n i) (+ (d i) (cont-frac-rec n d k (+ i 1))))))

(define (cont-frac n d k)
  ;(cont-frac-rec n d k 1))
  (cont-frac-iter n d k 0))

(define (phi k)
  (/ 1 (cont-frac (lambda (i) (1.0)) (lambda (i) (1.0)) k)))

(define (cont-frac-iter n d k frac)
  (if (= k 0)
      frac
      (cont-frac-iter n d (- k 1) (/ (n k) (+ (d k) frac)))))

; (phi 12) gives 1.6180555555555558; accurate by 4 decimal digits.

