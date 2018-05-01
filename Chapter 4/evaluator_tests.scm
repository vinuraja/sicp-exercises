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