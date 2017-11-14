(put 'zero? '(scheme-number)
     (lambda (x) (tag (= x 0))))

(define (zero-rat? x)
    (and (= (numer x) 0)
         (not (= (denom x) 0))))

(put 'zero? '(rational)
     (lambda (x) (tag (zero-rat? x))))

(define (zero-complex? z)
    (and (= (real-part z) 0)
         (= (imag-part z) 0)))

(put 'zero? '(complex)
     (lambda (z)
       (tag (zero-complex? z))))
