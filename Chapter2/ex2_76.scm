; Explicit dispatch: we would have to add explicit cases for each type in
; each operation. And for each operation a separate method with cases for
; all the types.
;
; Data-directed: we would have add rows in the table for each operation and
; columns for each type.
;
; Message-passing: we would have to add a new dispatch method for each type.
; And for operations we would have to add code to every type's dispatch method.

; Many types added: in this case, code wise both Data-directed and Message-passing
; would be the same amount of work, and explicit will take the most code. But between
; the former two, Message-passing would be better because the operations for the same
; type can be organized together in a coherent bunch of code.
;
; Many operations added: again, the reasoning is similar to above, and Message-passing
; seems more preferable.
