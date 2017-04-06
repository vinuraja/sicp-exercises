(define (+ a b)
  (if (= a 0) 
      b 
      (inc (+ (dec a) b))))
; Upon substitution we see that builds up a linearly recursive process.

(define (+ a b)
  (if (= a 0) 
      b 
      (+ (dec a) (inc b))))
; This builds up a linearly iterative process instead.
