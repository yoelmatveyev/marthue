(defpackage :cl-marthue
  (:use :common-lisp)
  (:export make-marthue-program
	   marthue-program-code
	   marthue-program-block
	   marthue-program-dir
	   marthue-program-mode
	   marthue-program-counter
	   marthue-program-step
	   marthue-program-stack
	   marthue-program-halted
	   marthue-program-line
	   marthue-program-newline
	   msearch
	   marthue-reset
	   marthue-run
	   load-thue-program
	   load-markov-program
	   run
	   load-run
	   load-program
	   ))

(in-package :cl-marthue)
	
