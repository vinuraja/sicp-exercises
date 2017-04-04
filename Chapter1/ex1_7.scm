(define (sqrt-iter prevguess guess x)
  (if (good-enough? prevguess guess x)
      guess
      (sqrt-iter guess (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y) 
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (good-enough? prevguess guess x)
;  (< (abs (/ (- prevguess guess) guess)) 0.001))
   (= guess (improve guess x))) ; At limited precision, this will become true.

(define (sqrt x)
  (sqrt-iter 0.0 1.0 x))
; This improves accuracy of sqrt for both small and large numbers.
