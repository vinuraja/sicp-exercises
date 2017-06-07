(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1)
                    list2))))

(define (same-parity-iter v values l)
  (cond ((null? values) l)
        ((or (and (even? v) (even? (car values)))
            (and (odd? v) (odd? (car values))))
          (same-parity-iter v (cdr values) (append l (list (car values)))))
        (else (same-parity-iter v (cdr values) l))))

(define (same-parity v . values)
  (same-parity-iter v values (list v))

