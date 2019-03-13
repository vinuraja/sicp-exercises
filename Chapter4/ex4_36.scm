#lang sicp

(#%require racket/include)
(include "../Chapter3/streams.scm")
(#%require (only sicp-pict amb))

; Replacing an-integer-between with an-integer-starting-from in the
; procedure a-pythagorean-triple-between won't work because the list
; of ks > j are infinite, and once k^2 > i^2 + j^2, we have no hope
; of finding that k, but we will keep on searching an infinite list.

(define (require p)
  (if (not p) (amb)))

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

(define (a-triple)
  (define (a-triple-impl triples-integers)
    (amb (stream-car triples-integers)
         (a-triple-impl (stream-cdr triples-integers))))
  (a-triple-impl (triples integers integers integers)))

(define (a-pythagorean-triple)
  (let ((triple (a-triple)))
    (require (= (square (caddr triple))
                      (+ (square (car triple)) (square (cadr triple)))))
    triple))
       
; A solution from https://www.inchmeal.io/sicp/ch-4/ex-4.36.html
; that I liked is presented below.
;
; IIRC I did go towards something like this in my thinking,
; but gave up when I saw I had to use an-integer-between. But
; when I looked at the solution, I found it very elegant. I
; should have pursued this line of thinking further.
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (an-integer-starting-from low)
  (amb low (an-integer-starting-from (+ low 1))))

(define (a-pythagorean-triple-from low)
  (let ((k (an-integer-starting-from low)))
    (let ((j (an-integer-between low k)))
      (let ((i (an-integer-between low j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))