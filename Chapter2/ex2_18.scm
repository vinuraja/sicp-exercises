(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define (reverse list1)
  (if (null? (cdr list1))
      list1
      (append (reverse (cdr list1)) (list (car list1)))))

