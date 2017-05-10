; Using fast-expt instead of expmod would be technically
; correct, but the runtime would be too high because we would
; actually be exponentiating. In the other cases we will always
; be dealing with numbers < m.

