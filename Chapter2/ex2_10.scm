(define (make-interval x y)
  (cons x y))

(define (upper-bound int)
  (if (> (car int) (cdr int))
      (car int)
      (cdr int)))

(define (lower-bound int)
  (if (<= (car int) (cdr int))
      (car int)
      (cdr int)))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (non-negative? x)
  (not (negative? x)))

(define (div-interval x y)
  (if (or (zero? (upper-bound y)) (zero? (lower-bound y))
           (and
             (positive? (upper-bound y))
             (negative? (lower-bound y))))
      (error "Divisor interval y spans zero.")
      (mul-interval x 
                    (make-interval 
                    (/ 1.0 (upper-bound y)) 
                    (/ 1.0 (lower-bound y))))))

