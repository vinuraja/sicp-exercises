(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (identity x)
  x)

(define (inc x)
  (+ x 1))

(define (pi-term x)
  (/ (* (+ x 1) (+ x 3)) (* (+ x 2) (+ x 2))))

(define (pi-next x)
  (+ x 2))
      
(define (approximate-pi)
  (* 4 (product pi-term 1.0 pi-next 1000)))

(define (product-iter term a next b p)
  (if (> a b)
      p
      (product-iter term (next a) next b (* p (term a)))))
      
(define (approximate-pi-iter)
  (* 4 (product-iter pi-term 1.0 pi-next 1000 1)))

