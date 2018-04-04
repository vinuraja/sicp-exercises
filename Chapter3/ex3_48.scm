#lang sicp
; Assume that there's a set of resources {Ra, Rb, ... } required by 2 processes.
; Deadlock is caused when some part of the set is acquired by one
; process, while the other part is acquired by the other. Now both
; are stuck waiting for each other to return the resources.
;
; If we set a total ordering on the access pattern by giving it a unique ordering,
; we can avoid deadlock, because it ensures that only one of the process
; can access all of the resources at the same time. So one process gets full access,
; then the other, effectively causing serialization.

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (id1 (account1 'id))
        (serializer2 (account2 'serializer))
        (id2 (account2 'id)))
    (if (< id1 id2)
        ((serializer1 (serializer2 exchange)) account1 account2)
        ((serializer2 (serializer1 exchange)) account1 account2))))