# Thue

Common Lisp interpreter for the esoteric Thue language meant to be used interractively in REPL.

An example of usage:

>  (load "thue.lisp")
>  (defvar roman (thue-load "lisp/thue/roman.t"))
>  (thue-run-pr roman)
"MDCCIII"
>
