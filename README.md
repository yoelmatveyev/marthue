# Marthue

This virtual machine impelements a superset of both semi-Thue systems and Markov algorithms extended by input/output, termination and conditional jump mechainisms. Programs are organized as lists of blocks. Each block may be either a sequental Markov-style or a non-deterministic semi-Thue-like algorithm. Normally, termination of each block passes the resulting string to the next one. Conditionals jump to labeled blocks may overwrite the normal order of the operation and serve as subroutines.

An Lispy interpreter of the Thue language, **thue.lisp**, which partly inspired this project, is added for historical purposes, although the Marthue engine includes its capabilities.

A collection of Thue and Markov programs (mostly not written by me) is added to the repository as examples.

The system is meant interractively in REPL.

An example of usage:

CL-MARTHUE> (defvar roman-thue (load-thue-program "examples/roman.t"))

ROMAN-THUE

CL-MARTHUE> (marthue-run roman-thue)

"MDCCIII"

CL-MARTHUE> (defvar roman-markov (load-markov-program "examples/roman.m"))

ROMAN-MARKOV

CL-MARTHUE> (marthue-run roman-markov)

"XVIII"

CL-MARTHUE> (marthue-program-counter roman-thue)

10687

CL-MARTHUE> (marthue-program-counter roman-markov)

23

CL-MARTHUE> (marthue-run roman-markov)


Program was terminated. Reset it?

(y or n) y

Try to run it again!

"******************"

CL-MARTHUE>
