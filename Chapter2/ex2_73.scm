(define h (make-hashtable length equal?))
(define (put op type item)
  (hashtable-set! h (list op type) item))
(define (get op type)
  (hashtable-ref h (list op type) #f))

(define (addend s) (car s))
(define (augend s) (cadr s))

(define (install-add-package)
  (define (deriv-add exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (put 'deriv '+ deriv-add))
(install-add-package)

(define (multiplier p) (car p))
(define (multiplicand p) (cadr p))

(define (install-multiply-package)
  (define (derive-multiply exp var)
    (make-sum
           (make-product 
            (multiplier exp)
            (deriv (multiplicand exp) var))
           (make-product 
            (deriv (multiplier exp) var)
            (multiplicand exp))))
  (put 'deriv '* derive-multiply))
(install-multiply-package)

(define (base exp) (car exp))
(define (exponent exp) (cadr exp))

(define (install-exponentiation-package)
  (define (derive-exponentiation exp var)
    (make-product
      (make-product
        (exponent exp)
        (make-exponentiation (base exp)
                             (make-sum (exponent exp) -1)))
      (deriv (base exp) var)))
  (put 'deriv '** derive-exponentiation))
(install-exponentiation-package)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation base exp)
    (cond ((=number? base 1) 1) 
          ((=number? exp 1) base) 
          ((=number? exp 0) 1) 
          (else (list '** base exp))))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; We are basically dispatching based on the operator of
; the expression. We could theoretically do this for number
; and variable too, but won't be the right abstraction.

; If dispatch is changed to:
; ((get (operator exp) 'deriv) 
;   (operands exp) var)
; then put needs to be correspondingly changed to:
; (put '+ 'deriv deriv-add)
