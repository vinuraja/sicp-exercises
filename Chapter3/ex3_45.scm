#lang sicp
; Yes, there is a problem in this, which can be illustrated
; using the serialized-exchange method, where we use the
; serializers from both accounts. Now exchange will go on to
; call deposit and/or withdraw from either account, but note
; that they are already serialized by one of these aforementioned
; serializers. But since we are already in the exchange method,
; serialized by both of them, the deposit/withdraw method won't
; be invoked since they use the same serializers, causing deadlock.
