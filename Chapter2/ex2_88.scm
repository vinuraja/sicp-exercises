#lang sicp
(put 'sub '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 (negate-poly p2)))))

(define (negate-poly p)
  (make-poly (variable p) (negate-term-list (term-list p))))

(define (negate-term-list l)
  (define (negate-term-list-iter l negl)
    (if (empty-term-list? l)
        (negl)
        (adjoin-term (negate-term (first-term l))
                     (negate-term-list-iter (rest-terms l) negl))))
  (negate-term-list-iter l '()))

; Instead of using -, negate can be made into a generic method and
; used here, so that we can support negating polynomial coefficients
; as well.
(define (negate-term term)
  (make-term (order term) (- (coeff term))))
