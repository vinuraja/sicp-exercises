#lang sicp
(define (rand-update x)
  (+ x 1))

(define rand
  (let ((x 0))
    (define (rnd)
      (set! x (rand-update x)) x)
    (define (reset value)
      (set! x value))
    (define (dispatch m)
      (cond ((eq? m 'generate) (rnd))
            ((eq? m 'reset) reset)
            (else (error "Unknown request: " m))))
    dispatch))