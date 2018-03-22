#lang sicp

     +---+     +---+    +---+
+--> | a +---> | b +--> | c +-----+
|    +---+     +---+    +---+     |
|                                 |
+---------------------------------+

; Trying to compute (last-pair z) will cause
; infinite recursion.
