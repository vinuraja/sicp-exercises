#lang sicp
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) 
            (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) 
                              (coeff t2)))
                  (new-o (- (order t1) 
                            (order t2))))
              (let ((q-term (make-term new-o new-c)))
                (let ((new-L1
                       (sub-terms L1 (mul-terms L2 (adjoin-term q-term '())))))
                  (let ((rest-of-result (div-terms new-L1 L2)))
                    (list (adjoin-term q-term (car rest-of-result))
                          (cadr rest-of-result))))))))))

(define (div-poly p1 p2)
  (if (same-variable? (variable p1) 
                      (variable p2))
      (let ((q-and-r-terms (div-terms (term-list p1) (term-list p2))))
        (list (make-poly (variable p1) (car q-and-r-terms))
              (make-poly (variable p1) (cadr q-and-r-terms))))
      (error "Polys not in same var: 
              DIV-POLY"
             (list p1 p2))))