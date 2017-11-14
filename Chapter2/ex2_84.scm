(define h (make-hashtable length equal?))
(define (put op type item)
  (hashtable-set! h (list op type) item))
(define (get op type)
  (hashtable-ref h (list op type) #f))

(define (raise-real x)
  (make-complex-from-real-imag x 0))

(put 'raise '(real)
     (lambda (x) (raise-real x)))

(define (raise-rat x)
  (make-real (/ (numer x) (denom x))))

(put 'raise '(rational)
     (lambda (x) (raise-rat x)))

(define (raise-scheme-number x)
  (make-rational x 1))

(put 'raise '(scheme-number)
       (lambda (x) (raise-scheme-number x)))

(define (raise-depth value)
  (define (raise-depth-iter val depth)
    (let ((raise-proc (get 'raise (map type-tag val))))
         (if raise-proc
             (raise-depth-iter (apply raise-proc val) (+ depth 1))
             depth)))
  (raise-depth-iter value 0))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((raise1-proc (get 'raise (list type1)))
                      (raise2-proc (get 'raise (list type2)))
                      (d1 (raise-depth a1))
                      (d2 (raise-depth a2)))
                  (cond ((> d1 d2)
                         (apply-generic 
                          op (raise1-proc a1) a2))
                        ((< d1 d2)
                         (apply-generic 
                          op a1 (raise2-proc a2)))
                        (else
                         (error 
                          "No method for 
                           these types"
                          (list 
                           op 
                           type-tags))))))
              (error 
               "No method for these types"
               (list op type-tags)))))))
