(define (raise-rat x)
  (make-complex-from-real-imag x 0))

(put 'raise '(rational)
       (lambda (x) (raise-rat x)))

(define (raise-scheme-number x)
  (make-rational x 1))

(put 'raise '(scheme-number)
       (lambda (x) (raise-scheme-number x)))
