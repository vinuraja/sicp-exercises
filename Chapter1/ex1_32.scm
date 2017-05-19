(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (identity x)
  x)

(define (inc x)
  (+ x 1))

(define (factorial n)
  (product identity 1 inc n))

(define (integer-sum n)
  (sum identity 1 inc n))

(define (accumulate-iter combiner term a next b combined)
  (if (> a b)
      combined
      (accumulate-iter combiner term (next a) next b
                  (combiner (term a) combined))))

(define (accumulate combiner null-value term a next b)
  (accumulate-iter combiner term a next b null-value))

