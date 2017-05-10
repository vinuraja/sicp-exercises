(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (process-time-clock)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 8)
      (report-prime (- (process-time-clock) 
                       start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (signal-sqrt remaining m)
  (if (= remaining 1)
      0
      remaining))

(define (signal-or-remainder possible-sqrt m)
  (if (or (= possible-sqrt 1) (= possible-sqrt (- m 1)))
      1
      (signal-sqrt (remainder (square possible-sqrt) m) m))) 

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (signal-or-remainder (expmod base (/ exp 2) m) m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (search-for-primes start end)
  (cond
    ((<= start end)
     (timed-prime-test start)
     (search-for-primes (+ start (if (even? start) 1 2)) end))
    (else (newline) (display " end\n "))))

; Predicts that the Carmichael numbers are not prime too!
