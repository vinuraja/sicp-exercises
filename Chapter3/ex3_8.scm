#lang sicp

(define f
  (let ((var -1))
    (lambda (val)
      (if (= var -1)
          (begin (set! var val) val)
          0))))