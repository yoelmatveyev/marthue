# Marthue

This virtual machine impelements a superset of both semi-Thue systems and Markov algorithms extended by input/output, termination and conditional jump mechanisms. A program is organized as a list or array of rule blocks. Each block may be either a sequental Markov-style or a non-deterministic Thue-like algorithm. Normally, termination of each block passes the resulting string to the next one. Calling labeled blocks as fuctions and returning from the fuction calls overwrites the normal order of the operation and adds a fuctional programming flavor to the machine.

A collection of Thue and Markov programs (mostly not written by me) is added to the repository as examples.

The system is meant interractively in REPL.

For more information read **INFO.md**

An example of usage:

Load the system:

CL-USER> (asdf:operate 'asdf:load-op 'marthue)

CL-USER> (in-package :cl-marthue)

Or without the package:

CL-USER> (load "marthue.lisp")

Load three versions of the Roman number converter:

CL-MARTHUE> (defparameter roman-thue (load-thue-program "examples/thue/roman.t"))

ROMAN-THUE

CL-MARTHUE> (marthue-run roman-thue)

"MDCCIII"

CL-MARTHUE> (defparameter roman-markov (load-markov-program "examples/markov/roman.m"))

ROMAN-MARKOV

CL-MARTHUE> (marthue-run roman-markov)

"XVIII"

CL-MARTHUE>  (defparameter roman-marthue (load-marthue-program "examples/marthue/roman.mt.lisp"))

ROMAN-MARTHUE

CL-MARTHUE> (marthue-run roman-marthue)

Instead of the packaged version, you can just load the file **marthue-no-package.lisp**.

You can also load and run programs by the shorthand macros **(load-run)** and **(load-program)**. The function **(marthue-run)** can be called by its alias **(run)**.

CL-MARTHUE> (load-run program1 "program1")

CL-MARTHUE> (run program1)
