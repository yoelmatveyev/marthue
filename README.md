# Lispy Thue

Common Lisp interpreter for the esoteric Thue language meant to be used interractively in REPL.

An example of usage:

CL>  (load "thue.lisp")

CL>  (defvar roman (thue-load "examples/roman.t"))

CL>  (thue-run-pr roman)

"MDCCIII"

CL> (load "examples/aroman.t.lisp")

CL> (thue-run-pr aroman.t)

"MDCCIII"

CL>
