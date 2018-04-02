#lang sicp
; Invariant is at least one account has $10, one has $20, and one has $30.
; After exchange happens, the same invariant holds. Any number of exchanges
; can happen sequentially and this will hold by induction.
;
; Peter might compute the difference in the balances for a1 and a2,
; but then Paul might change the balance in a1 before Peter is able to
; complete the exchange. Let's say, we were going to exchange $10 and $20,
; and we computed the difference as -$10, but before that $30 was exchanged
; with $20. So when we apply the difference, we get $20, $20 and $20. The
; total sum will remain $60 because the individual account transactions are
; serialized, so across the 3 accounts no amount is withdrawn or deposited.
;
; Even this condition can be broken if the individual account transactions
; are not serialized. For example if $10 and $20 are being exchanged
; concurrently with $20 and $30, then the $20 account could be operated on
; concurrently, where we could have one of the withdrawal/deposits from one
; of the exchanges overriding the other one and the balance being left as
; let's say $20, $30 and $20.