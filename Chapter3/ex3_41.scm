#lang sicp
; Very observant of Ben. Yes, unserialized access to balance
; could potentially cause anomalous behaviour if balance is
; being accessed as the same time as one of the other methods
; is updating it. This depends on how balance is stored and
; accessed by the interpreter and the underlying hardware.
