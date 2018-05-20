(#%require rackunit)

(check-equal? #t (eval '(and) genv))
(check-equal? 1 (eval '(and 1) genv))
(check-equal? 0 (eval '(and 1 0) genv))
(check-equal? 1 (eval '(and 0 1) genv))

(check-equal? #f (eval '(or) genv))
(check-equal? 1 (eval '(or 1) genv))
(check-equal? 1 (eval '(or 1 0) genv))
(check-equal? 0 (eval '(or 0 1) genv))

(check-equal? 3 (eval (+ 1 2) genv))
(check-equal? '(+ 1 2) (eval ''(+ 1 2) genv))
(check-equal? 3 (eval '(+ 1 2) genv))

(check-equal? 5 (eval '(cond (1 (+ 2 3)) (else false)) genv))
(check-equal? 2 (eval '(cond ('(b 2) => cadr) (else false)) genv))

(check-equal? 3 (eval '((lambda (x) (+ 1 x)) 2) genv))
(check-equal? 3 (eval '((lambda (x y) (+ y x)) 1 2) genv))
(check-equal? 3 (eval '(let ((x 2)) (+ 1 x)) genv))
(check-equal? 3 (eval '(let ((x 2) (y 1)) (+ y x)) genv))
(check-equal? 39 (eval
                  '(let* ((x 3)
                          (y (+ x 2))
                          (z (+ x y 5)))
                     (* x z)) genv))
(check-equal? 8 (eval '((lambda (n)
                          (let fib-iter ((a 1) (b 0) (count n))
                            (if (= count 0)
                                b
                                (fib-iter (+ a b) 
                                          a 
                                          (- count 1)))))
                        6)
                      genv))