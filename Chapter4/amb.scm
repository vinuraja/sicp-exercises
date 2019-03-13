;#lang sicp

(#%require (only sicp-pict amb))

(define require-count 0)

(define (require p)
  (set! require-count (+ require-count 1))
  (if (not p) (amb)))

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