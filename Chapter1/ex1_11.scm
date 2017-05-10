(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1)) (* 2 (f-recur (- n 2))) (* 3 (f-recur (- n 3))))
  )
)

(define (f-iter n)
  (f-iter-iter 2 1 0 n)
)

(define (f-iter-iter a b c n)
  (if (= n 0)
      c
      (f-iter-iter (+ a (* 2 b) (* 3 c)) a b (- n 1))
  )
)
