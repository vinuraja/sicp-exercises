(define (make-rat n d)
  (if (positive? d)
      (cons n d)
      (cons (- n) (- d))))

