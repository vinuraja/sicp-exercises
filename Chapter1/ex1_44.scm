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

(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
     (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

; So do ((n-fold-smooth inc 4) 5) to get 6
(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

