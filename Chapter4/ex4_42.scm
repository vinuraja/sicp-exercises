#lang sicp

(#%require racket/include)
(include "amb.scm")

(define (xor a b)
  (or (and (not a) b)
      (and a (not b))))

; Answer is {{betty 3} {ethel 5} {joan 2} {kitty 1} {mary 4}}.
(define (liars-puzzle)
  (let* ((positions (amb-sequence (rkt:in-permutations (rkt:list 1 2 3 4 5))))
         (betty (rkt:list-ref positions 0))
         (ethel (rkt:list-ref positions 1))
         (joan (rkt:list-ref positions 2))
         (kitty (rkt:list-ref positions 3))
         (mary (rkt:list-ref positions 4)))
    (require (and (xor (= kitty 2) (= betty 3))
                  (xor (= ethel 1) (= joan 2))
                  (xor (= joan 3) (= ethel 5))
                  (xor (= kitty 2) (= mary 4))
                  (xor (= mary 4) (= betty 1))))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))
