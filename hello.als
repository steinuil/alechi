; I don't want to worry about syntax right now, best to start with simple
; s-expressions.
(include "stdio.h")

(typedef ayy_int int)

(decl ayy_str "const char *"
  ('string "ayy lmao"))

(proc main () ayy_int
  (for ((decl i int 0) (<= (+ 1 i) 10) (++ i))
    (ignore (printf ('string "%d\n") i)))
  (ignore (puts ayy_str))
  (return ('int 0)))
