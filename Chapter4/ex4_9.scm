#lang sicp
; You could have a functional while syntax as:
; (while pred body init-args)
; where pred is a lambda which takes a list of args,
; and returns a boolean, based on which we continue
; or terminate the loop, and return the incoming value
; of the args. Body is a lambda which takes a list of
; args and returns a possibly modified list of args.
; Init-args is the list of args with their initial values.
;
; We can convert this into a definition in the global
; environment like so:
; (define (while (pred body init-args)
;   ; This part can be used as a derived expression instead
;   ; as well, but name collisions would need to be solved
;   ; somehow.
;   (define (while-impl args)
;     (if (pred args)
;       (while-impl (body args))
;       args))
;   (while-impl init-args))
