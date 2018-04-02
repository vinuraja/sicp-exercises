#lang sicp
; Yes, this method will work for the problem of just transferring
; an amount from one account to another. Note that we shouldn't
; allow negative withdrawals/deposits and withdraw should be done
; first, so that we can prevent overdrafts, for this to be strictly
; correct.
;
; This doesn't work for the exchange problem, because there is an
; eseential difference that for the exchange problem, we have to
; read the balances first to get the difference between the accounts.
; If one of the balances change from below us, then the difference
; we calculated becomes wrong, and the exchange becomes incorrect.