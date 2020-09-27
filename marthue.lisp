(in-package :cl-marthue)

;; The main structure that stores the Marthue program and its state 

(defstruct marthue-program
  code
  block
  dir
  mode
  counter
  block-counter
  step
  stack
  halted
  line
  newline)

;; Substring replacement (from the Common Lisp Cookbook)

(defun replace-all (string part replacement &key (test #'char=))
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

;; Forward, backward and random substring search

(defun msearch (l1 l2 &optional (dir 1))
  (let (found p)
  (case dir
    (1 (search l1 l2))
    (-1 (search l1 l2 :from-end t))
    (otherwise
     (loop for x from 0 to (- (length l2) (length l1)) do
	  (setf p (search l1 l2 :start2 x))
	  (if p
	      (progn (push p found) (setq x p))
	      (return)))
     (if found (nth (random (length found)) found) nil)))))

;; Determing the search direction

(defun rule-dir (dir rand rule)
  (let (d
	(b (member :B rule))
	(f (member :F rule)))
    (setf d (if dir dir
	(if rand 0 1)))
    (if (and b f) 0
	(if b -1
	    (if f 1
		d)))))

;; Auxiliary function for stepping through a program block

(defun block-step (rules line &key dir rand debug)
  (let*  ((l (length rules))
	  (n nil) (r (random l)) s1 s2 rule p d c v s)
    (if rand
	   (case (random 2)
	     (0 (loop for x from r to (+ r l -1) do
		     (setf
		      rule (elt rules (mod x l))
		      d (rule-dir dir rand rule)
		      n (msearch (car rule) line d))
		     (when n (setf c (mod x l)) (return n))))
	     (1 (loop for x from r downto (- r l +1) do
		     (setf
		      rule (elt rules (mod x l))
		      d (rule-dir dir rand rule)
		      n (msearch (car rule) line d))
		     (when n (setf c (mod x l)) (return n)))))
	   (loop for x from 0 to (1- l) do
		(setf
		 rule (elt rules x)
		 d  (rule-dir dir rand rule)
		 n (msearch (car rule) line d))
		(when n (setf c x) (return n))))
    (when n
      (setf s1 (subseq line 0 n)
	    s2 (subseq line (+ n (length (car rule))))
	    p (cadr rule))
      (when (member :O rule)
	(format t "~a" (cadr rule))
	(setf p nil))
      (when (member :I rule)
	(setf p
	      (case dir
		(-1 (concatenate 'string (read-line) p))
		(1  (concatenate 'string p (read-line))))))
      (setf line (concatenate 'string s1 p s2)))
    (if debug (progn
		(format t "~%Dir:~a Random:~a Rule:~a" d rand c)
		(print rule)
		(format t "~%~a~%" line)))
    (setf s
	  (remove nil (list (if (find :N rule) :N)
		(if (find :R rule) :R)
		(find-if-not #'constantp rule))))
    (if (and n s)
	(setf v s)
	(setf v (if n nil '(:N))))
    (values line v c)))

;; Is the block Markov-line or Thue-like?

(defun block-mode (block)
  (if (member :T (car block)) t))

;; What's the default search direction of the block?

(defun block-dir (block)
  (let* ((a (if (block-mode block) 0 1))
	(c (car block))
	(b (member :B c))
	(f (member :F c)))
    (if (and b f) 0
	(if b -1
	    (if f 1
		a)))))

;; Resetting the program

(defun marthue-reset (pr &key code line)
  (when code
    (setf (marthue-program-code pr) code))
  (when line
    (setf (marthue-program-line pr) line)) 
    (setf (marthue-program-stack pr) nil
	  (marthue-program-step pr) 0
	  (marthue-program-block pr) 0

	  (marthue-program-counter pr) 0
	  (marthue-program-halted pr ) nil
	  (marthue-program-dir pr ) (block-dir
				     (elt (marthue-program-code pr)
					  (marthue-program-block pr)))
	  (marthue-program-newline pr ) (marthue-program-line pr)
	  (marthue-program-mode pr) (block-mode
				     (elt (marthue-program-code pr)
					  (marthue-program-block pr))))
    t)

;; Auxiliary function for termination conditions and jumping between blocks

(defun marthue-update (pr status &key debug)
  (let* (
	 (block (marthue-program-block pr))
	 (newblock block)
	 (blength (length (marthue-program-code pr)))
	 lbl term)
    (when (and (listp status) (find :N status))
      (incf newblock)
      (setf term t))
	(if (and (listp status) (find-if-not #'constantp status))
	    (if (find :R status) 
		(let (c n (l (car (last status))))
		     (if l
			 (if (marthue-program-stack pr)
			     (loop for x from 0 to (1- (length (marthue-program-stack pr))) do
				  (setf n (nth x (marthue-program-stack pr)))
				  (when (find l (car (elt (marthue-program-code pr) n))
					      :test #'string=)
				    (if (/= n block)
					(progn (setf
						(marthue-program-stack pr)
						(subseq (marthue-program-stack pr) x)
						c (car (marthue-program-stack pr))
						newblock (if (numberp c) c blength))
					       (pop (marthue-program-stack pr)))
					(if (marthue-program-stack pr)
					    (setf newblock (pop (marthue-program-stack pr)))
					    (setf newblock blength)))
				    (return)))
			     (setf newblock blength))
			 (if (marthue-program-stack pr)
			     (setf newblock (pop (marthue-program-stack pr)))
			     (setf newblock blength))))
		(let ((l (car (last status))))      
		  (loop for x from 0 to (1- blength) do
		       (if (find l (car (elt (marthue-program-code pr) x)) :test #'string=)
			   (push x lbl)))
	      (when lbl
		(setf newblock (elt lbl (random (length lbl))))
		(if (and (/= block newblock) (not term)) 
		    (push block (marthue-program-stack pr))))
	      (unless lbl
		(setf newblock blength)))))
    (if debug
	(format t "~%Status change:~a Block:~a Next block:~a Stack:~a~%"
		status block newblock (marthue-program-stack pr)))
    (if (< newblock blength)
	    (setf 
	     (marthue-program-step pr) 0
	     (marthue-program-block pr) newblock
	     (marthue-program-dir pr ) (block-dir
					(elt (marthue-program-code pr)
					     (marthue-program-block pr)))
	     (marthue-program-mode pr) (block-mode
					(elt (marthue-program-code pr)
					     (marthue-program-block pr))))
	    (setf (marthue-program-halted pr) t))
    nil))

;; The main engine

(defun marthue-run (pr &key (steps -1) debug fulldebug)
  (let* ((halted (marthue-program-halted pr))
	 blk mode dir rules (status nil))
    (when halted
      (format t "~%Program was terminated. Reset it?")
      (when (y-or-n-p)
	(marthue-reset pr)
	(format t "Try to run it again!~%")))
    (loop while (and (/= steps 0)(null halted)) do
	 (loop while (and (/= steps 0)(null status)) do
	      (setf blk (marthue-program-block pr)
		    rules (cadr (elt (marthue-program-code pr) blk))
		    mode (block-mode (elt (marthue-program-code pr) blk))
		    dir (block-dir (elt (marthue-program-code pr) blk)))
	 (multiple-value-bind (l v c)
	     (block-step rules
			 (marthue-program-newline pr)
			 :rand mode :dir dir :debug fulldebug)
	   (setf (marthue-program-newline pr) l
		 (marthue-program-step pr) c
		 status v)
	   (incf (marthue-program-counter pr))
	   (decf steps)))
	      (if status (setf status (marthue-update pr status :debug (or debug fulldebug))
			       halted (marthue-program-halted pr)))
	      (if (zerop steps)
		  (return  (marthue-program-newline pr))))
    (marthue-program-newline pr)))

;; Convert a Thue program into a Marthue structure

(defun load-thue-program (file)
  (let (p r l res (c (make-array 1 :adjustable t)))
    (with-open-file (stream file)
      (setf p (loop for l = (read-line stream nil)
		 while l
		 collect l)))
    (setf p (remove "" p :test #'equal)
	  p (remove "::=" p)
	  r (subseq p 0 (- (length p) 2))
	  l (nth (1- (length p)) p)
	  r (mapcar (lambda (x)
		      (let* ((n (search "::=" x))
			     (s (subseq x (+ n 3)))
			     (h (list (subseq x 0 n) s)))
			     (if (equal s ":::") (setf h (append h '(:I))))
			     (if (and (>= (length s) 1)(equal (subseq s 0 1) "~"))
				 (setf h (append h '(:O))
				       (cadr h) (subseq x (+ n 4))))
				 h))
		    r))
    (setf
     (aref c 0) (list '(:T) (coerce r 'vector)) 
     res (make-marthue-program)
     (marthue-program-code res) c
     (marthue-program-line res) l)
     (marthue-reset res)
    res))

;; Convert a Markov algorithm into a Marthue structure

(defun load-markov-program (file)
  (let (p r l res (c (make-array 1 :adjustable t)))
    (with-open-file (stream file)
      (setf p (loop for l = (read-line stream nil)
		 while l
		 collect l)))
    (setf p (remove "" p :test #'equal)
	  r (subseq p 0 (- (length p) 1))
	  l (nth (1- (length p)) p)
	  r (mapcar (lambda (x)
		      (let* (h
			     (n (search "->" x))
			     (s (subseq x (+ n 2))))
			(if (equal (subseq s 0 1) ".")
			    (setf h (list (subseq x 0 n) (subseq s 1) "."))
			    (setf h (list (subseq x 0 n) s)))
			h))
		    r))
    (setf
     (aref c 0) (list '(:M) (coerce r 'vector)) 
     res (make-marthue-program)
     (marthue-program-code res) c
     (marthue-program-line res) l)
     (marthue-reset res)
     res))

;; Auxiliary function for splitting the Marthue code into blocks

(defun split-code (l delimiter1 delimiter2)
  (let (pr bl)
    (loop for x from 0 to (length l) do
	 (when (and (search delimiter1 (nth x l))
		    (not (search delimiter2 (nth x l))))
	   (loop for y from (1+ x) to (length l)
	      do
		(if (or (search delimiter2 (nth y l)))
		    (push (nth y l) bl)
		    (progn
		      (push (list (nth x l)(reverse bl)) pr)
		      (setf bl nil x (1- y))
		      (return))))))
    (reverse pr)))

;; Getting label/opcode information from a string

(defun string-opcode (s &key (opcode nil))
  (let (lbl type (sc (search "::" s)))
    (if sc
      (progn (unless opcode (setf s (subseq s 0 sc)))
      (setf type (subseq s 0 (search " " s))
	    lbl (search " " s :from-end t)
	    lbl (if lbl (make-symbol (string-upcase (subseq s (1+ lbl)))) nil)
	    type (if type type s)
	    type (remove nil
			 (remove-duplicates
			  (list (if (search "M" type) :M)
				(if (search "T" type) :T)
				(if (search "B" type) :B)
				(if (search "F" type) :F)
				(if (search "X" type) :B)
				(if (search "X" type) :F)
				(if (search "I" type) :I)
				(if (search "O" type) :O)
				(if (search "N" type) :N)
				(if (search "R" type) :R)
				(if (search "C" type) :C)))))
      (remove nil (append type (list lbl))))
      nil)))

(defun remove-backslash (s)
  (replace-all
   (replace-all
    (replace-all
     (replace-all
      s
      "\-" "-")
     "\>" ">")
    "\." ".")
   "\\\n" (string #\newline)))

;; Convert a replacement rule to Lisp

 (defun rule-to-lisp (x)
   (let (n s)
     (if (setf n (search "->." x))
	 (setf s (subseq x (+ n 3)))
	 (setf n (search "->" x)
	       s (subseq x (+ n 2))))
     (list
      (remove-backslash (subseq x 0 (search "->" x)))
      (remove-backslash s))))

;; Convert a Marthue algorithm from a file

(defun load-marthue-program (file)
  (let (p l res)
    (with-open-file (stream file)
      (setf p (loop for l = (read-line stream nil)
		 while l
		 collect l)))
    (setf l (nth (1- (length p)) p))
    (if (or (search "::" l)(search "->" l))
	(setf l "")
	(setf p (subseq p 0 (- (length p) 1))))
    (setf p (split-code
	    (remove-if-not (lambda (x) (or (search "::" x)(search "->" x))) p) "::" "->")
	  p (mapcar (lambda (x)
		      (let ((m (string-opcode (car x))))
		      (list (if m m '(:M))
			    (mapcar (lambda (y)
				      (let ((z (search "::" y)))
					(append
					 (rule-to-lisp (if z (subseq y (+ z 2)) y))
					 (if (search "->." y) (list :N))
					 (string-opcode y))))
				    (cadr x)))))
		    p))
    (setf res (make-marthue-program)
	  (marthue-program-code res) p
	  (marthue-program-line res) l)
    (marthue-reset res)
    res))

;; Load a Marthue program as Lisp code

(defun load-lisp-marthue (prg)
  (let
      ((st (make-marthue-program))
       code line)
    (if (stringp prg)
	(with-open-file (in prg)
	  (setf code (loop for x = (read in nil)
			until (null x) collect x)
		line (cadr code)
		code (car code)))
	(setf code prg))
    (setf (marthue-program-code st) code
	  (marthue-program-line st) line)
    (marthue-reset st)
    st))
