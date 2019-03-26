;#lang sicp

(#%require (only sicp-pict amb))

(define require-count 0)

(define (require p)
  (set! require-count (+ require-count 1))
  (if (not p) (amb)))

(#%require (prefix rkt: racket/base))
(#%require (prefix rkt: racket/list))
(#%require (prefix rkt: racket/sequence))
(#%require (prefix rkt: racket/stream))

(define (amb-stream s)
  (require (not (rkt:stream-empty? s)))
  (amb (rkt:stream-first s) (amb-stream (rkt:stream-rest s))))

(define (amb-sequence s)
  (amb-stream (rkt:sequence->stream s)))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (distinct? lst)
  (define (distinct-elem? el)
    (let ((eq-cnt (accumulate (lambda (x acc)
                                (if (equal? el x)
                                    (+ acc 1)
                                    (+ acc 0)))
                              0 lst)))
      (= eq-cnt 1)))
  (define (distinct?-impl l)
    (cond ((null? l) #t)
          ((distinct-elem? (car l)) (distinct?-impl (cdr l)))
          (else #f)))
  (distinct?-impl lst))

(#%require rackunit)

(check-true (distinct? '(1)))
(check-false (distinct? '(1 1)))
(check-true (distinct? '(1 2 3)))
(check-false (distinct? '(1 2 3 1)))
(check-false (distinct? '('(1 2) 1 '(1 2))))
(check-true (distinct? '('(1) '(1 2) '(1 2 3))))