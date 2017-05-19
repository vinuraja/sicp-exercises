(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess count)
    (display guess)
    (display " ")
    (display count)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ count 1)))))
  (try first-guess 1))

(define (exp-x-damping)
  (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2.0))

(define (exp-x)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))

; The damping function takes around 9 guesses whereas the non-damping
; function takes about 34 guesses to converge!

