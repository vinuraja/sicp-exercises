#lang sicp
(#%require racket/include)
(include "streams.scm")

(define (Ramanujan-numbers-method)
  (define (cube x) (* x x x))
  (define (cube-sum p)
    (+ (cube (car p)) (cube (cadr p))))
  (define (cube-ordered)
    (weighted-pairs integers integers cube-sum))
  (define (cubes)
    (stream-map cube-sum (cube-ordered)))
  ; We have an implementation where we have the cubes stream be iterated
  ; 3 ways, each skipping an item, and checking if the middle one equals
  ; to the previous one, and not equal to the next one. This way, we can
  ; find repeating cubes, even if they repeat more than once.
  (define (impl)
    (define cubes-impl (stream-map (lambda (a b c) (list a b c))
                                   (cubes)
                                   (stream-cdr (cubes))
                                   (stream-cdr (stream-cdr (cubes)))))
    (stream-map (lambda (l)
                  (cadr l)) (stream-filter
                             (lambda (l)
                               (and (= (car l) (cadr l)) (not (= (cadr l) (caddr l)))))
                             cubes-impl)))
  (impl))
    

(define Ramanujan-numbers (Ramanujan-numbers-method))

(define (iter n)
  (if (not (= n -1))
      (begin (iter (- n 1)) (display (stream-ref Ramanujan-numbers n)) (newline))))
(iter 5)
;1729
;4104
;13832
;20683
;32832
;39312