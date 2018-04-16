#lang sicp
; This version isn't efficient because there is redundant calculation
; of (sqrt-stream x) till n-1 if we are trying to calculate
; (stream-ref (sqrt-stream x n)).
; The difference between both implementations in terms of efficiency
; would go away if memoization was absent.