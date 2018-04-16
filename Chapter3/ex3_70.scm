#lang sicp
(#%require racket/include)
(include "streams.scm")

; Procedures merge-weighted and weighted-pairs have been
; added to streams.scm, so that they can be re-used in
; other exercises.

(define int-weighted-pairs
  (weighted-pairs integers integers (lambda (p)
                                      (+ (car p) (cadr p)))))

(define (divisible-by? n x)
  (= 0 (remainder x n)))

(define not-div-integers
  (stream-filter (lambda (x)
                   (not (or
                         (divisible-by? 2 x)
                         (divisible-by? 3 x)
                         (divisible-by? 5 x))))
                 integers))

(define not-div-int-weighted-pairs
  (weighted-pairs not-div-integers not-div-integers
                  (lambda (p)
                    (+ (* 2 (car p)) (* 3 (cadr p)) (* 5 (car p) (cadr p))))))
