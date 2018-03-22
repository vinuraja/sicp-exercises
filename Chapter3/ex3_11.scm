#lang sicp
; The local state for acc will be kept in an environment E1
; with the parent being the global environment. E1 will have
; balance as 50 initially.
; The local states for acc and acc2 will be kept distinct, because
; acc2 will have its local state stored in a separate environemnt
; E2. E1 and E2 will share the same parent environment, and also
; the body of their procedures.