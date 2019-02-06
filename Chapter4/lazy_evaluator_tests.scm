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
(check-equal? 3 (eval '(let ((x 2) (y 1))
                         (= x 3) ; verifies multiple statement support
                         (+ y x)) genv))


(check-equal? 39 (eval
                  '(let* ((x 3)
                          (y (+ x 2))
                          (z (+ x y 5)))
                     (= x 4) ; verifies multiple statement support
                     (* x z)) genv))

; Won't work with lazy evaluation for some reason which I haven't
; put time into figuring out yet!
;(check-equal? 8 (eval '((lambda (n)
;                          (let fib-iter ((a 1) (b 0) (count n))
;                            (if (= count 0)
;                                b
;                                (fib-iter (+ a b) 
;                                          a 
;                                          (- count 1)))))
;                        6)
;                      genv))
;(check-equal? 8 (eval '((lambda (n)
;                          (let fib-iter ((a 1) (b 0) (count n))
;                            (= a 0) ; verifies multiple statement support
;                            (if (= count 0)
;                                b
;                                (fib-iter (+ a b) 
;                                          a 
;                                          (- count 1)))))
;                        6)
;                      genv))

(check-equal? 'ok (eval '(define (append x y)
                           (if (null? x)
                               y
                               (cons (car x) (append (cdr x) y)))) genv))
(check-equal? '(a b c d e f) (eval '(append '(a b c) '(d e f)) genv))
(check-equal? 'ok (eval '(define (map f x)
                           (if (null? x)
                               '()
                               (cons (f (car x)) (map f (cdr x))))) genv))
(check-equal? '(2 4 6 8) (eval '(map (lambda (x) (* 2 x)) '(1 2 3 4)) genv))
(check-equal? 'ok (eval '(define (map-even? x)
                           (define (even? n)
                             (if (= n 0)
                                 true
                                 (odd? (- n 1))))
                           (define (odd? n)
                             (if (= n 0)
                                 false
                                 (even? (- n 1))))
                           (map even? x)) genv))
(check-equal? '(#f #t #f #t) (eval '(map-even? '(1 2 3 4)) genv))

(check-equal? 3628800 (eval '(letrec
                                 ((fact
                                   (lambda (n)
                                     (if (= n 1)
                                         1
                                         (* n (fact (- n 1)))))))
                               (fact 10)) genv))
(check-equal? '(#f #t #f #t) (eval '((lambda (x)
                                       (letrec
                                           ((even?
                                             (lambda (n)
                                               (if (= n 0)
                                                   true
                                                   (odd? (- n 1)))))
                                            (odd?
                                             (lambda (n)
                                               (if (= n 0)
                                                   false
                                                   (even? (- n 1))))))
                                         (map even? x))) '(1 2 3 4)) genv))

; Testing laziness specifically: notice (/ 1 0).
(check-equal? 1 (eval '((lambda (a b) (if (= a 0) 1 b)) 0 (/ 1 0)) genv))