#lang sicp
; The new-value in the action-procedure is created when the action
; is scheduled, at the time of input's set-signal!. So if we don't
; follow the FIFO order (and follow LIFO), the final output value
; would be incorrect.
;
; For example, in the case of an and-gate's inputs changing from (0, 1) to
; (1, 0), let's say inputs changed in that order first 1 and then 0. If we
; go by LIFO order, first the output changes to 0, and then to 1. But the
; actual output should be 0 for inputs (0, 1). Only the FIFO order provides
; the right output of 0.