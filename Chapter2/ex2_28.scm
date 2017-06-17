(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define (fringe val)
  (cond ((null? val) val)
        ((not (pair? val)) (list val))
        (else (append (fringe (car val)) (fringe (cdr val))))))

