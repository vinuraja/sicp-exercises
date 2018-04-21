#lang sicp
(#%require racket/include)
(include "streams.scm")

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo experiment-stream 
                     passed 
                     failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) 
      passed 
      failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (integral-estimates P x1 y1 x2 y2)
  (define (points-stream)
    (cons-stream (list x1 y1 x2 y2) (points-stream)))
  (define (exp-stream)
    (stream-map (lambda (p)
                  (let ((x (random-in-range (car p) (caddr p)))
                        (y (random-in-range (cadr p) (cadddr p))))
                    (P x y))) (points-stream)))
  (stream-map (lambda (area-ratio)
                (* area-ratio (- x2 x1) (- y2 y1))) (monte-carlo (exp-stream) 0 0)))

(define (pi-estimates)
  (define (pred x y)
   (<= (+ (* x x) (* y y)) 1.0))
  (integral-estimates pred -1.0 -1.0 1.0 1.0))

; (stream-ref (pi-estimates) 10000000)
; => 3.1422720857727913
