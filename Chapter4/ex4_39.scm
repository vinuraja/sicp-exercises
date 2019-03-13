#lang sicp

(#%require racket/include)
(include "amb.scm")

; The order of the restrictions shouldn't affect the solution
; because the solution is supposed to adhere to all restrictions,
; like a logical 'and'.

; The order of the restrictions should affect the time to find an
; answer. If a particular restriction can cause a choice to be discarded,
; it makes sense to try that restriction first for that choice, leading
; to skipping the other restrictions. So, the more discriminative
; restrictions should be prioritized over the less discriminative.
; For example re-arranging the restrictions for multiple-dwelling as
; presented below gives a require-count (number of require methods called)
; of 1604 for the answer. The multiple-dwelling definition in the book has
; a require-count of 1716.

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
      (distinct? (list baker cooper fletcher 
                       miller smith)))
    (require
      (not (= (abs (- smith fletcher)) 1)))
    (require 
      (not (= (abs (- fletcher cooper)) 1)))
    (require (> miller cooper))
    (require (not (= fletcher 1)))
    (require (not (= fletcher 5)))
    (require (not (= cooper 1)))
    (require (not (= baker 5)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))