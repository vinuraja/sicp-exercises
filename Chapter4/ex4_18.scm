#lang sicp
; The transformation in the exercise won't work for the
; integral example because when we try to evaluate
; (b (stream-map f y), we will lookup variable y, which
; will be unassigned (recall that we did have a as the integral
; currently.
; If we use the transformation in the text, y is the integral
; and dy is (stream-map f y), and since the integral uses
; (delay dy) we are fine.