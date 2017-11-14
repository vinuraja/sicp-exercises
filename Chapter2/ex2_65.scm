(define (union-list-iter list1 list2 result)
  (cond ((null? list1) (append result list2))
        ((null? list2) (append result list1))
        (else (let ((x1 (car list1)) (x2 (car list2)))
                   (cond ((< x1 x2)
                             (union-list-iter (cdr list1) list2 (append result (list x1))))
                         ((> x1 x2)
                             (union-list-iter list1 (cdr list2) (append result (list x2))))
                         (else (union-list-iter (cdr list1) list2 result)))))))

(define (inter-list-iter list1 list2 result)
  (if (or (null? list1) (null? list2))
      result
      (let ((x1 (car list1)) (x2 (car list2)))
           (cond ((= x1 x2)
                     (inter-list-iter (cdr list1) (cdr list2) (append result (list x1))))
                 ((< x1 x2) (inter-list-iter (cdr list1) list2 result))
                 (else (inter-list-iter list1 (cdr list2) result))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (div (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

(define (union-set set1 set2)
  (list->tree (union-list-iter (tree->list-2 set1) (tree->list-2 set2) '())))

(define (intersection-set set1 set2)
    (list->tree (inter-list-iter (tree->list-2 set1) (tree->list-2 set2) '())))

; For ease of testing
(define (union-set-list list1 list2)
  (union-set (list->tree list1) (list->tree list2)))

(define (intersection-set-list list1 list2)
  (intersection-set (list->tree list1) (list->tree list2)))
