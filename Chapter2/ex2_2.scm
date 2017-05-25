(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (half-point coord start-point end-point)
  (/ (+ (coord start-point) (coord end-point)) 2))

(define (midpoint-segment segment)
  (make-point
    (half-point x-point (start-segment segment) (end-segment segment))
    (half-point y-point (start-segment segment) (end-segment segment))))

