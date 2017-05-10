(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (square x)
  (* x x))

(define (fib-iter a b p q count)
  (cond ((= count 0) 
         b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))  ;compute p'
                   (+ (* 2 p q) (square q q))  ;compute q'
                   (/ count 2)))
        (else 
         (fib-iter (+ (* b q) 
                      (* a q) 
                      (* a p))
                   (+ (* b p) 
                      (* a q))
                   p
                   q
                   (- count 1)))))(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((even? b) (* 2 (fast-mul a (/ b 2))))
        (else (+ a (fast-mul a (- b 1))))))
