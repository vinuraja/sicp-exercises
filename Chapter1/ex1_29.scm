(define (simpsons-rule f a b n)
  (if (>= a b)
      0
      (* (simpsons-rule-sum f a b (/ (- b a ) n) n 0) (/ (- b a) (* 3 n)))))
    
(define (simpsons-rule-sum f a b h n count)
  (if (= count (+ n 1))
      0
      (+ (* (multiplier n count) (y f a h count)) 
        (simpsons-rule-sum f a b h n (+ count 1)))))

(define (y f a h count)
  (f (+ a (* count h))))

(define (multiplier n count)
  (cond ((or (= count 0) (= count n)) 1)
        ((even? count) 2)
        (else 4)))

(define (cube x)
  (* x x x))

; Using (simpsons-rule cube 0 1 n) gives rational numbers.
; n = 100 => 1/4
; n = 1000 => 1/4 
; Can use (simpsons-rule cube 0 1.0 n) to get floating point
; n = 100 => .24999999999999992
; n = 1000 => .2500000000000003

