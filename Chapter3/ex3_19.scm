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
    (define (has-cycle-impl? x y first-time)
      (cond ((or (null? x) (null? y) (null? (cdr y))) #f)
            ; If we have looped back onto ourselves. :)
            ((and (not first-time) (eq? x y)) #t)
            (else (has-cycle-impl? (cdr x) (cddr y) #f))))
    (has-cycle-impl? x x #t))