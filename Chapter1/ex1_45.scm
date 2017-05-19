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

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    ;(newline)
    ;(display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (nth-root n x)
  (fixed-point 
   (n-fold-average-damp 
    (lambda (y) (/ x (expt y (- n 1))))
    ; Dampen it log2(n) times.
    (floor (/ (log n) (log 2))))
   1.0))

