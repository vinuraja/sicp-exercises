
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (get z 2 1 0))

(define (cdr z)
  (get z 3 1 0))

(define (get z factor p e)
  (if (= (remainder (/ z p) factor) 0)
      (get z factor (* p factor) (+ e 1))
      e))

