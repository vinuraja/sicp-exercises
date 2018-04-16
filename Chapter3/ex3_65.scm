#lang sicp
(#%require racket/include)
(include "streams.scm")

(define (ln2-summands n)
  (cons-stream 
   (/ 1.0 n)
   (stream-map - (ln2-summands (+ n 1)))))

(define (partial-sums s)
  (define partial-sums-impl
    (cons-stream (stream-car s) (add-streams partial-sums-impl (stream-cdr s))))
  partial-sums-impl)

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))     ; Sn-1
        (s1 (stream-ref s 1))     ; Sn
        (s2 (stream-ref s 2)))    ; Sn+1
    (cons-stream 
     (- s2 (/ (square (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream 
   s
   (make-tableau
    transform
    (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define accelerated-ln2-stream
  (accelerated-sequence euler-transform ln2-stream))

(stream-ref accelerated-ln2-stream 2)
; returned is 0.6932773109243697
; expected is 0.69314718056
; so it is accurate to first 3 decimal places.
