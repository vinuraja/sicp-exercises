(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define (reverse list1)
  (if (or (null? list1) (not (pair? list1)))
      list1
      (append (reverse (cdr list1)) (list (car list1)))))

(define (identity x)
  x)

(define (deep-reverse val)
  (if (or (null? val) (not (pair? val)))
      val
      (append (deep-reverse (cdr val)) (list (deep-reverse (car val))))))

