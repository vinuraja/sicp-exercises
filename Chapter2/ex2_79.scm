(put 'equ? '(scheme-number scheme-number)
     (lambda (x y) (tag (= x y))))

(define (equ-rat? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))

(put 'equ? '(rational rational)
       (lambda (x y) (tag (equ-rat? x y))))

(define (equ-complex? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))

(put 'equ? '(complex complex)
     (lambda (z1 z2)
       (tag (equ-complex? z1 z2))))
