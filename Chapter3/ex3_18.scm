#lang sicp
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (contains l v)
  (cond ((null? l) #f)
        ((eq? (car l) v) #t)
        (else (contains (cdr l) v))))
  
(define (has-cycle? x)
  (let ((pairs '()))
    (define (has-cycle-impl? x)
      (cond ((null? x) #f)
            ((contains pairs x) #t)
            (else (set! pairs (append pairs (list x)))
                  (has-cycle-impl? (cdr x)))))
    (has-cycle-impl? x)))