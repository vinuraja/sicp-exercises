(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (process-time-clock)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (process-time-clock) 
                       start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n a)
  (= (expmod a n n) a))

(define (prime? n)
  (prime?-iter n (- n 1)))

(define (prime?-iter n a)
  (cond ((= a 0) true)
        ((fermat-test n a) 
         (prime?-iter n (- a 1)))
        (else false)))
