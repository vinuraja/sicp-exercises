#lang sicp
(#%require racket/include)
(include "streams.scm")

(define int-pairs (pairs integers integers))

; index(i, j):
;   if i = j     is  2^i - 2
;      i = j + 1 is  3 * 2^(i - 1) - 2
;      i > j + 1 is  3 * 2^(i - 1) - 2 + 2^i * (j - i - 1)
(define (index i j)
  (+ (- (expt 2 i) 2)
     (cond ((= i j) 0)
           ((= i (+ j 1)) (expt 2 (- i 1)))
           (else (+ (expt 2 (- i 1)) (* (expt 2 i) (- (- j i) 1)))))))

(define (test-index i j)
  (let ((indices (stream-ref int-pairs (index i j))))
    (and (= i (car indices))
         (= j (cadr indices)))))

(index 1 100)   ; 197
(index 99 100)  ; 950737950171172051122527404030
(index 100 100) ; 1267650600228229401496703205374