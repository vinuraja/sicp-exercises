(define (accumulate-iter filter? combiner null-value term a next b combined)
  (define (filtered-term-a)
    (if (filter? a)
        (term a)
        null-value))
  (if (> a b)
      combined
      (accumulate-iter filter? combiner null-value term (next a) next b
                  (combiner (filtered-term-a) combined))))

(define (filtered-accumulate filter? combiner null-value term a next b)
  (accumulate-iter filter? combiner null-value term a next b null-value))

(define (all x)
  #t)

(define (sum filter? term a next b)
  (filtered-accumulate filter? + 0 term a next b))

(define (product filter? term a next b)
  (filtered-accumulate filter? * 1 term a next b))

(define (identity x)
  x)

(define (inc x)
  (+ x 1))

(define (factorial n)
  (product all identity 1 inc n))

(define (integer-sum n)
  (sum all identity 1 inc n))

(define (square x)
  (* x x))

(define (sum-prime-squares a b)
  (sum prime? square a inc b))

(define (gcd m n) 
  (cond ((< m n) (gcd n m)) 
        ((= n 0) m) 
        (else (gcd n (remainder m n)))))

(define (sum-relative-primes n)
  (define (relative-prime? x)
    (= 1 (gcd x n)))
  (sum relative-prime? identity 1 inc n))

