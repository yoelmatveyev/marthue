;; The main structure that store the Marthue program and its state 

(defstruct marthue-program
  code
  block
  dir
  mode
  counter
  step
  stack
  halted
  line
  newline)

;; Forward, backward and random substring search

(defun msearch (l1 l2 &optional (dir 1))
  (case dir
    (1 (search l1 l2))
    (-1 (search l1 l2 :from-end t))
    (otherwise 
     (let (n a (l (length l1)))
       (loop for x from 0 to (- (length l2) l) do
	    (unless (setf n (search l1 l2 :start2 x))
	      (return))
	    (when n (push n a)
		     (setf x n)))
       (if a
	   (nth (random (length a)) a)
	   nil)))))

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
	    s2 (subseq line (+ n (length (car rule)))))
      (if (member :O rule)
	  (progn (format t "~a" (cadr rule))
		 (setf p "")))
      (if (member :I rule)
	      (setf p (concatenate 'string (read-line))) (setf p (cadr rule)))
      (setf line (concatenate 'string s1 p s2)))
    (if debug (progn
		(format t "~%Dir:~a Random:~a Rule:~a" d rand c)
		(print rule)
		(format t "~%~a~%" line)))
    (setf s (caddr rule))
    (if (and n (stringp s))
	(setf v s)
	(setf v (if n nil t)))
    (values line v c)))

;; Is the block Markov-line or Thue-like?

(defun block-mode (block)
  (if (member :T (car block)) t))

;; What's the default search diection of the block?

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
	 lbl)
    (if (eq status t)
	(if (not (marthue-program-stack pr))
	    (incf newblock)
	    (setf newblock (pop (marthue-program-stack pr))))
	(loop for x from 0 to (1- blength) do
	 (if (find status (car (elt (marthue-program-code pr) x)) :test #'equal)
	     (push x lbl))))
    (if lbl
	(progn
	  (setf newblock (elt lbl (random (length lbl))))
	  (if (/= block newblock) 
	       (push block (marthue-program-stack pr))))
	(progn
	  (when (equal status ".") (setf newblock blength))
	  (when (equal status "?")
	    (setf newblock (random blength))
		  (push newblock (marthue-program-stack pr)))
	  (when (equal status "-")
	    (if (plusp block)
		(decf newblock)
		(push newblock (marthue-program-stack pr))))
	  (when (equal status "+")
	    (incf newblock)
	    (push newblock (marthue-program-stack pr)))))
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
    
		    
