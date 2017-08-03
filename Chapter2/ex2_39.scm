(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define fold-right accumulate)

(define nil (list))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define (reverse-right sequence)
  (fold-right 
   (lambda (x y)
     (if (null? y)
         (list x)
         (append y (list x)))) nil sequence))

(define (reverse-left sequence)
  (fold-left 
   (lambda (x y)
     (if (null? x)
         (list y)
         (append (list y) x)))
    nil sequence))

