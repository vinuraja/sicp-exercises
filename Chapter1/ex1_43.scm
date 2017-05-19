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

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

; ((repeated square 2) 5) => 625
; ((repeated inc 4) 5) => 9

