; Efficiency is same as non-repeated representation.
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; Efficiency is O(1) vs O(n) of non-repeated representation.
(define (adjoin-set x set)
  (cons x set))

; Efficiency is O(n) vs O(n^2) of non-repeated representation.
(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set-iter set1 set2 result)
  (cond ((or (null? set1) (null? set2)) result)
        ((and (element-of-set? (car set2) set1))
            (intersection-set-iter set1 (cdr set2) (append result (list (car set2)))))
        (else (intersection-set-iter set1 (cdr set2) result))))

; Efficiency is O(n^2) vs O(n^2); same as non-repeated representation.
(define (intersection-set set1 set2)
  (intersection-set-iter set1 set2 '()))
