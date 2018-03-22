#lang sicp
(define (contains l v)
  (cond ((null? l) #f)
        ((eq? (car l) v) #t)
        (else (contains (cdr l) v))))
  
(define (count-pairs x)
  (let ((pairs '()))
    (define (count-impl x)
      (if (or (not (pair? x)) (contains pairs x))
          0
          (begin (set! pairs (append pairs (list x)))
                 (+ (count-impl (car x)) (count-impl (cdr x)) 1))))
    (count-impl x)))