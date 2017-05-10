(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((even? b) (* 2 (fast-mul a (/ b 2))))
        (else (+ a (fast-mul a (- b 1))))))
