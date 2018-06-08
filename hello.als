; I don't want to worry about syntax right now, best to start with simple
; s-expressions.
(include "stdio.h")
(include_rel "test.h")

(typedef ayy int)

(proc main () ayy
  (ignore (puts ('string "ayy lmao")))
  (return ('int 0)))
