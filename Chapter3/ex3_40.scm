#lang sicp
; All possibilities:
; 1,000,000: P1 before P2, or P2 before P1.
; 100      : P1 and P2 reads x as 10 and calculates
;            there individual functions, and then P1
;            writes it's result last.
; 1000     : Similar to above, but P2 writes result last.
; 100,000  : P1 sets x as 100, but P2 reads one x as 10,
;            and rest as 100.
; 10,000   : Similar to above, but P2 reads two x-es as 10.
;            Or P2 sets x as 1000, but P1 reads one x as 10,
;            and the other as 1000.

; Remaining possibilities:
; 1,000,000.