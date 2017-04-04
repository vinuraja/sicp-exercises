(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))
; This will cause infinite recursion because sqrt-iter will be evaluated first
; according to the applicative order when evaluating new-if.
