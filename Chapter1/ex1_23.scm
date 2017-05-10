(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) 
                       start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

; next as per ex1_23.
(define (next n)
  (if (= n 2)
    (+ n 1)
    (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (search-for-primes start end)
  (cond
    ((<= start end)
     (timed-prime-test start)
     (search-for-primes (+ start (if (even? start) 1 2)) end))
    (else (newline) (display " end\n "))))

; Ratios are roughly 2, but not exactly; maybe due to
; overhead like the if-condition, function calling, etc.
