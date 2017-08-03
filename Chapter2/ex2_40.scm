(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define nil (list))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate-interval 
             (+ low 1)
             high))))

(define (square x)
  (* x x))

(define (smallest-div n) 
  (define (divides? a b) 
    (= 0 (mod b a))) 
  (define (find-div n test) 
      (cond ((> (square test) n) n) ((divides? test n) test) 
            (else (find-div n (+ test 1))))) 
  (find-div n 2)) 
  
(define (prime? n) 
  (if (= n 1) false (= n (smallest-div n))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap
         (lambda (i)
           (map (lambda (j) 
                  (list i j))
                (enumerate-interval 
                 1 
                 (- i 1))))
         (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter 
        prime-sum?
        (unique-pairs n))))

