#lang sicp
(#%require rackunit)

(check-equal? ((lambda (n)
                 ((lambda (fact) (fact fact n))
                  (lambda (ft k)
                    (if (= k 1)
                        1
                        (* k (ft ft (- k 1)))))))
               10) 3628800)

(check-equal? ((lambda (n)
                 ((lambda (fib) (fib fib 1 0 n))
                  (lambda (fb a b count)
                    (if (= count 0)
                        b
                        (fb fb (+ a b) a (- count 1))))))
               6) 8)
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) 
         true 
         (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) 
         false 
         (ev? ev? od? (- n 1))))))

(check-equal? (f 10) #t)