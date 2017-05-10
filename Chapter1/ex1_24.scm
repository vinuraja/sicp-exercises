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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (search-for-primes start end)
  (cond
    ((<= start end)
     (timed-prime-test start)
     (search-for-primes (+ start (if (even? start) 1 2)) end))
    (else (newline) (display " end\n "))))

; Hard to measure difference in timings when using fermat-test
; because log(n) runtime makes it too fast for modern computers.
; Tried changing runtime to process-time-clock even!

