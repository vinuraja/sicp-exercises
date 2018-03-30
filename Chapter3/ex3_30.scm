#lang sicp
(define (ripple-carry-adder A B S C)
  (define (add-ripples A B Cin S)
    (cond ((and (null? (cdr A)) (null? (cdr B)) (null? (cdr S)))
           (full-adder (car A) (car B) Cin (car S) C))
          ((or (null? (cdr A)) (null? (cdr B)) (null? (cdr S)))
           (error "At least one of the lists is shorter than the others."))
          (else (let ((Cout (make-wire)))
                  (full-adder (car A) (car B) Cin (car S) Cout)
                  (add-ripples (cdr A) (cdr B) Cout (cdr S))))))
  (add-ripples A B 0 S))
           