#lang sicp
(#%require racket/include)
(include "streams.scm")

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (p)
                              (list (stream-car s) (car p) (cadr p)))
                            (stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (square x) (* x x))

(define pythagorean-triples
  (stream-filter (lambda (triple)
                   (= (square (caddr triple))
                      (+ (square (car triple)) (square (cadr triple)))))
                 (triples integers integers integers)))
                 