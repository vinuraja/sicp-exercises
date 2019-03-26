#lang sicp

(#%require racket/include)
(include "amb.scm")

(define barnacle (cons 1 'barnacle))
(define downing (cons 2 'downing))
(define hall (cons 3 'hall))
(define moore (cons 4 'moore))
(define parker (cons 5 'parker))

(define (father d)
  (rkt:car d))
(define (yacht-owner d)
  (rkt:cadr d))
(define (a-father-and-yacht-owner)
  (let* ((combinations (rkt:sequence->stream
                        (rkt:in-combinations (rkt:list barnacle downing hall moore parker) 2)))
         (all-combinations (rkt:stream-append combinations
                                              (rkt:stream-map (lambda (fando)
                                                                (rkt:list (rkt:cadr fando)
                                                                          (rkt:car fando)))
                                                              combinations))))
    (amb-stream all-combinations)))

(define (parkers-daughter dlist)
  (cond ((null? dlist) '())
        ((equal? (father (car dlist)) parker) (car dlist))
        (else (parkers-daughter (cdr dlist)))))

(define (distinct-choices? d1 d2)
  (and (not (equal? (father d1) (father d2)))
       (not (equal? (yacht-owner d1) (yacht-owner d2)))))

; 1. {lornas-father downing}
; 2. 2 solutions:
; {lornas-father parker}
; {lornas-father downing}
(define (yacht-problem)
  (let ((mellisa (a-father-and-yacht-owner)))
    (require (equal? (father mellisa) barnacle))
    (require (equal? (yacht-owner mellisa) downing))
    (let ((mary (a-father-and-yacht-owner)))
      ; Comment out the below line to get the answer to the 2nd part of the
      ; question.
      (require (equal? (father mary) moore))
      ; This is redundant for the 1st part of the question, but useful in the
      ; 2nd part.
      (require (distinct-choices? mary mellisa))
      (let ((gabrielle (a-father-and-yacht-owner)))
        (require (equal? (yacht-owner gabrielle) barnacle))
        (require (distinct-choices? gabrielle mary))
        (require (distinct-choices? gabrielle mellisa))
        (let ((lorna (a-father-and-yacht-owner)))
          (require (equal? (yacht-owner lorna) moore))
          (require (distinct-choices? lorna gabrielle))
          (require (distinct-choices? lorna mary))
          (require (distinct-choices? lorna mellisa))
          (let ((rosalind (a-father-and-yacht-owner)))
            (require (equal? (yacht-owner rosalind) hall))
            (require (distinct-choices? rosalind lorna))
            (require (distinct-choices? rosalind gabrielle))
            (require (distinct-choices? rosalind mary))
            (require (distinct-choices? rosalind mellisa))
            (require (equal? (yacht-owner (parkers-daughter
                                           (list mellisa mary gabrielle lorna rosalind)))
                             (father gabrielle)))
            (list 'lornas-father (cdr (father lorna)))))))))
                                                          
