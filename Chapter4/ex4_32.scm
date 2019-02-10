#lang sicp
; The biggest difference being that cons is now delayed for both
; car and cdr. This allows us to create lazy trees as well, which
; can be traversed in any order, without necessarily strictly evaluating
; the cars.