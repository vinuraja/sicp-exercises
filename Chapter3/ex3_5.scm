#lang sicp
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) 
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) 
                 trials-passed))))
  (iter trials 0))

(define (estimate-integral P x1 y1 x2 y2 trials)
  (define (exp)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
        (P x y)))
  (let ((area-ratio (monte-carlo trials exp)))
      (* area-ratio (- x2 x1) (- y2 y1))))

(define (estimate-pi trials)
  (define (pred x y)
   (<= (+ (* x x) (* y y)) 1.0))
  (estimate-integral pred -1.0 -1.0 1.0 1.0 trials))

; (estimate-pi 100000000)
; 3.14168632