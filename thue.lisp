(defstruct thue-program
  rules
  line)

(declaim (inline search-random thue-step thue-run thue-run-rp))

(defun search-random (l1 l2)
  (declare (optimize (speed 3) (safety 0)(debug 0)))
  (let (n a (l (length l1)))
   	  (loop for x from 0 to (- (length l2) l) do
	       (unless (setf n (search l1 l2 :start2 x))
		 (return))
	       (when n (push n a)
		     (setf x n)))
	  (if a
	      (nth (random (length a)) a)
	      nil)))

(defun thue-step (rules line)
  (declare (optimize (speed 3) (safety 0)(debug 0)))
  (let*  ((l (length rules))
	 (r (random (length rules)))
	 (n nil) x s1 s2 i rule p)
    (setf
     x
	  (case (random 2)
	    (0 (loop for x from r to (+ r l -1) do
		 (setf n (search-random (car (elt rules (mod x l))) line))
		    (if n (return x))))
	    (1 (loop for x from r downto (- r l +1) do
		 (setf n (search-random (car (elt rules (mod x l))) line))
		    (if n (return x))))))
    (when n (setf rule (elt rules (mod x l))))
    (if n
	(progn
	  (setf i (caddr rule)
		s1 (subseq line 0 n)
		s2 (subseq line (+ n (length (car rule)))))
	  (case i
	    (:I (setf p (concatenate 'string (read-line))))
	    (:O (progn (format t "~a" (cadr rule))))
	    (otherwise (setf p (cadr rule))))		
	  (setf line (concatenate 'string s1 p s2)))
	nil)))

(defun thue-run (rules line &optional (steps -2))
  (let (newline)
    (loop while (/= steps -1) do
	 (setf newline line
	       line (thue-step rules line))
	 (decf steps)
	 (unless line (return)))
    newline))

(defun thue-run-pr (pr &optional (steps -2))
  (thue-run (thue-program-rules pr)
	    (thue-program-line pr)
	    steps))

(defun thue-load (file)
  (let (p r l)
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
    (make-thue-program
     :rules (coerce r 'vector)
     :line l)))
		    
