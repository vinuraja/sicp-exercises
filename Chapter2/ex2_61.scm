(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))
        ((= x (car set)) set)
        (else (cons x set))))
