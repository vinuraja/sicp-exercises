(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (repeated-iter f n f))

(define (repeated-iter f n composed-f)
  (if (= n 1)
      (lambda (x)
        (composed-f x))
      (repeated-iter f (- n 1) (compose f composed-f))))

(define (average-damp f)
  (lambda (x) 
    (/ (+ x (f x)) 2)))

(define (n-fold-average-damp f n)
  ((repeated average-damp n) f))

(define tolerance 0.00001)
(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) 
      tolerance))

(define (fixed-point f first-guess)
  ((iterative-improve close-enough? f) first-guess))

(define (iterative-improve close-enough? improve)
  ; <mindblow> This shows recursion with lambdas! :) </mindblown>
  ((lambda (f) (f f))
   (lambda (try-gen)       
     (lambda (guess)
     ;(newline)
     ;(display guess)
     (let ((next (improve guess)))
       (if (close-enough? guess next)
           next
           ((try-gen try-gen) next)))))))

(define (nth-root n x)
  (fixed-point 
   (n-fold-average-damp 
    (lambda (y) (/ x (expt y (- n 1))))
    (floor (/ (log n) (log 2))))
   1.0))

(define (sqrt x)
  (define (improve-sqrt guess)
    (/ (+ guess (/ x guess)) 2))
  ((iterative-improve close-enough? improve-sqrt) 1.0))

; (expt (sqrt 625) 2) => 625
; (expt (nth-root 16 625) 16) => 625.0000000101835

