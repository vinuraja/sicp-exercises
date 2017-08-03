#lang sicp
(#%require sicp-pict)

(define (split bigger-op smaller-op)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller
               ((split bigger-op smaller-op) painter (- n 1))))
          (bigger-op painter
                     (smaller-op smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))