#lang sicp

(#%require racket/include)
(include "amb.scm")

(define (safe? row col rest-of-queens)
  (if (null? rest-of-queens)
      #t
      (let ((prev-row (caar rest-of-queens))
            (prev-col (cdar rest-of-queens)))
        (and (not (= row (caar rest-of-queens)))
             (not (= col (cdar rest-of-queens)))
             (not (= (abs (- row prev-row)) (abs (- col prev-col))))
             (safe? row col (cdr rest-of-queens))))))

; (queens 8)
; {{4 . 1} {2 . 2} {7 . 3} {3 . 4} {6 . 5} {8 . 6} {5 . 7} {1 . 8}}
(define (queens board-size)
  (define (queen-cols col rest-of-queens)
    (if (= col 0)
        rest-of-queens
        (let ((row (an-integer-between 1 board-size)))
          (require (safe? row col rest-of-queens))
          (queen-cols (- col 1) (append (list (cons row col)) rest-of-queens)))))
  (queen-cols board-size '()))