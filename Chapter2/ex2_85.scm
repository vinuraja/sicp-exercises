(define h (make-hashtable length equal?))
(define (put op type item)
  (hashtable-set! h (list op type) item))
(define (get op type)
  (hashtable-ref h (list op type) #f))

(define (project-complex x)
  (make-real (real-part x)))

(put 'project '(complex)
       (lambda (x) (project-complex x)))

(define (project-real x)
  (make-rational (numberator (rationalize x)) (denominator (rationalize x))))

(put 'project '(real)
     (lambda (x) (project-real x)))

(define (project-rat x)
  (make-scheme-number (numer x)))

(put 'project '(rational)
     (lambda (x) (project-rat x)))

(define (drop x)
  (if (equ? (raise project x) x)
      (drop (project x))
      x))

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
                  (if ((= type1 type2)
                         (error 
                          "No method for 
                           these types"
                          (list 
                           op 
                           type-tags))
                         (apply-generic 
                          op (drop a1) (drop a2)))))
              (error 
               "No method for these types"
               (list op type-tags)))))))
