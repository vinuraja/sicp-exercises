#lang sicp
(#%require sicp-pict)

(define (make-vect x y)
  (list x y))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame-cons origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (origin-frame-cons frame)
  (car frame))

(define (edge1-frame-cons frame)
  (cadr frame))

(define (edge2-frame-cons frame)
  (cddr frame))
