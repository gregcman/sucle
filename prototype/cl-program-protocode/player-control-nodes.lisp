(in-package :sandbox)

  (progno
   (defparameter node nil)
   (defparameter directions (alexandria:circular-list :up :left :down :right))
   (let ((moved? nil)
	 (last-node node)) 
     (progno
      (with-hash-table-iterator (next e:*keypress-hash*)
	(loop (multiple-value-bind (more key value) (next)
		(if more
		    (let ((ans (keyword-ascii key value)))
		      (whEn ans
			(when (e::r-or-p (e::get-press-value value))
			  (setf moved? t)
			  (node-splice
			   (node-left node)
			   (vector-circular-node
			    (string (code-char ans)))))))
		    (return))))))
     
     (when (skey-r-or-p :up)
       (block nil
	 (setf node (or (node-up node)
			(return)))
	 (setf moved? t)))
     (when (skey-r-or-p :down)
       (block nil
	 (setf node (or (node-down node)
			(return)))
	 (setf moved? t)))
     (cond ((skey-p :right-control)
	     ;;;long jumps
	    (when (skey-r-or-p :s)
	      (block nil
		(setf node (or (node-left (jump-car node))
			       (node-left node)
			       (return)))
		(setf moved? t)))
	    (when (skey-r-or-p :f)
	      (block nil
		(setf node (or (node-right (jump-cdr node))
			       (node-right node)
			       (return)))
		(setf moved? t))))
	   ((or (skey-p :right-alt)
		(skey-p :left-alt))
	    ;;character
	    (when (skey-r-or-p :s)
	      (block nil
		(setf node (or (prev-newline node)
			       (return)))
		(setf moved? t)))
	    (when (skey-r-or-p :f)
	      (block nil
		(setf node (or (next-newline node)
			       (return)))
		(setf moved? t))))
	   ((skey-p :left-shift)
	    (progn
	      (when (skey-r-or-p :s)
		(block nil
		  (setf node (or (node-left node)
				 (return)))
		  (setf moved? t)))
	      (when (skey-r-or-p :f)
		(block nil
		  (setf node (or (node-right node)
				 (return)))
		  (setf moved? t)))))
	   (t
	     ;;;cons cells
	     ;;;left moves to previous car
	     ;;;right moves to next cdr
	    (when (skey-r-or-p :s)
	      (block nil
		(setf node (or 
			    (labels ((find-car (node &optional (count 256))
				       (when node
					 (unless (zerop count)					  
					   (let ((payload (node-payload (node-up node))))
					     (let ((type (car (car payload))))
					       (if (and (eq type (quote car))
					;    (atom (car (cdr payload)))
							)
						   node						  
						   (let ((car (jump-car node)))
						     (if (and (atom (cdr (node-payload (node-up car))))
							      (eq (quote cdr) type))	       
							 car
							 (find-car (node-left node) (1- count)))))))))))
			      (find-car (if (eq (quote car)
						(car (car (node-payload (node-up node)))))
					    (node-left node)
					    node)))
			    (return)))
		(setf moved? t)))
	    (when (skey-r-or-p :f)
	      (block nil
		(setf node (or 
			    (labels ((find-cdr (node &optional (count 256))
				       (when node
					 (unless (zerop count)
					   (let ((payload (node-payload (node-up node))))
					     (let ((type (car (car payload))))
					       (if (and (eq (quote cdr) type)
					;   (atom (car (cdr payload)))
							)
						   node
						   (if (and (atom (cdr payload))
							    (eq (quote car) type))
						       (jump-cdr node)
						       (find-cdr (node-right node) (1- count))))))))))
			      (find-cdr (if (eq (quote cdr)
						(car (car (node-payload (node-up node)))))
					    (node-right node)
					    node)))
			    (return)))
		(setf moved? t)))))
     (progn (when (skey-r-or-p :d)
	      (block nil
		(setf node (or (short-down node)
			       (next-newline node nil)
			       (return)))
		(setf moved? t)))
	    (when (skey-r-or-p :e)
	      (block nil
		(setf node (or (short-up node)
			       (prev-newline node nil)
			       (return)))
		(setf moved? t))))

     (when (skey-r-or-p :kp-4)
       (setf moved? t)
       (let ((payload (node-payload node)))
	 (let ((newline (cdr payload)))
	   (when Newline
	     (decf (cdr payload))
	     (width-prop (node-right node) -1)))))
     (when (skey-r-or-p :kp-6)
       (setf moved? t)
       (let ((payload (node-payload node)))
	 (let ((newline (cdr payload)))
	   (when Newline
	     (incf (cdr payload))
	     (width-prop (node-right node) 1)))))

     (when (skey-r-or-p :kp-5)
       (setf moved? t)
       (let ((payload (node-payload node)))
	 (let ((newline (cdr payload)))
	   (if Newline
	       (progn
		 (setf (cdr payload) nil)
		 (width-prop (node-right node) (- Newline))) 
	       (setf (cdr payload) 0)))))

     (when (skey-r-or-p :backspace)
       (setf moved? t)
       (let ((ans (node-left node)))
	 (node-disconnect ans)))
     (when (skey-r-or-p :j)
       (print (node-payload (node-up node)))
       (print (node-payload node)))
     (when (skey-p :l)
       (dobox ((x 0 228)
	       (y 0 70))
	      (scwu (random most-positive-fixnum) x y)))
     (when (or t (skey-p :o)))
     (progn
       (when (skey-r-or-p :kp-enter)
	 (setf moved? t)
	 (setf node (turn-node node))
	 (pop directions)
	 (copy-string-to-world 0 5 (symbol-name (car directions)) *white-black-color*)))
     (when moved?
					;      (clear-screen)
       (unless (eq node last-node)
	 (setf (car (node-payload node))
	       (let ((char (car (node-payload node))))
		 (if (typep char (quote character))
		     (setf char (char-code char)))
		 (typecase char
		   (fixnum (let (( a (logand 255 char)))
			     (logior a (random-color)))))))
	 (setf (car (node-payload last-node))
	       (let ((char (car (node-payload last-node))))
		 (if (typep char (quote character))
		     (setf char (char-code char)))
		 (typecase char
		   (fixnum (let ((a (logand 255 char)))
			     (logior a *white-black-color*)))))))
       (draw-nodal-text *node-start* 0 0 1 -1 nil 1024)
       (draw-nodal-text (reverse-node *node-start*) 0 0 1 -1 t 1024))))

(progno
 (defun prev-newline (node &optional (after t))
   (labels ((rec (lastnode node &optional (cap 1024))
	      (if node
		  (unless (zerop cap)
		    (let ((newline (cdr (node-payload node))))
		      (if (typep newline (quote fixnum))
			  (if after lastnode node)
			  (rec node
			       (node-left node)
			       (1- cap)))))
		  lastnode)))
     (rec (node-left node)
	  (node-left (node-left node)))))

 (defun next-newline (node &optional (before t))
   (labels ((rec (lastnode node &optional (cap 1024))
	      (if node
		  (unless (zerop cap)
		    (let ((newline (cdr (node-payload node))))
		      (if (typep newline (quote fixnum))
			  (if before node lastnode)
			  (rec node
			       (node-right node)
			       (1- cap)))))
		  lastnode)))
     (rec (node-right node)
	  (node-right (node-right node)))))

 (defun short-down (node &optional (cap 1024))
   (labels ((node-newline (node)
	      (let ((newline (cdr (node-payload node))))
		(if (typep newline (quote fixnum))
		    Newline
		    0)))
	    (rec (node offset)
	      (when node
		(unless (zerop cap)
		  (decf cap) 
		  (if (zerop offset)
		      node
		      (rec (node-right node)
			   (1+ (+ offset (node-newline node)))))))))
     (rec (node-right node) (1+ (node-newline node)))))

 (defun short-up (node &optional (cap 1024))
   (labels ((node-newline (node)
	      (let ((newline (cdr (node-payload node))))
		(if (typep newline (quote fixnum))
		    newline
		    0)))
	    (rec (node offset)
	      (when node
		(unless (zerop cap)
		  (decf cap)
		  (decf offset (node-newline node))
		  (if (zerop offset)
		      node
		      (rec (node-left node) (1- offset)))))))
     (let ((left-node (node-left node)))
       (rec left-node -1))))
 
 (defun jump-cdr (node)
   (node-down (node-right (node-up node))))
 (defun jump-car (node)
   (node-down (node-left (node-up node))))

 (defun draw-nodal-text (node x y dx dy reversep &optional (count 32))
   (block nil
     (flet ((draw-forward ()
	      (dotimes (counter count)
		(unless node
		  (return))
		(let ((payload (node-payload node)))
		  (scwu node x y)
		  (let ((linefeed (cdr payload)))
		    (when (typep linefeed 'fixnum)
		      (incf x linefeed)
		      (incf y dy)))
		  (pop node))
		(incf x dx)))
	    (draw-backwards ()
	      (dotimes (counter count)
		(unless node
		  (return))
		(let ((payload (node-payload node)))
		  (unless (zerop counter)
		    (let ((linefeed (cdr payload)))
		      (when (typep linefeed 'fixnum)
			(decf x linefeed)
			(decf y dy))))
		  (scwu node x y)
		  (pop node))
		(decf x dx))))
       (if reversep
	   (draw-backwards)
	   (draw-forward)))))

 (defun clear-screen (&optional (rect *cam-rectangle*))
   (etouq
    (with-vec-params (vec-slots :rectangle
				(quote ((x0 :x0)
					(y1 :y1)
					(x1 :x1)
					(y0 :y0))))
      (quote (rect let))
      (quote
       (progn
	 (dobox ((x (floor x0) (ceiling x1))
		 (y (floor y0) (ceiling y1)))
		(scwu nil x y)))))))

 (setf *print-case* :downcase)

 (defparameter *cell-character-buffer* (make-array 0
						   :adjustable t
						   :fill-pointer 0
						   :element-type 'character))

 (defun print-cells2 (form &optional (chars *cell-character-buffer*))
   (let* ((start (make-cons-node #\())
	  (end start)
	  (counter 0))
     (labels ((attach-char-node (node)
		(node-connect-right end node)
		(setf end node)
		(incf counter))
	      (attach-char-and-place (char node)
		(let ((new-node (make-cons-node char)))
		  (node-connect-up new-node node)
		  (attach-char-node new-node)
		  new-node))
	      (prin1-and-done (object parent-node)
		(setf (fill-pointer chars) 0)
		(with-output-to-string (stream chars)
		  (prin1 object stream))
		(let ((len (fill-pointer chars)))
		  (let ((first-node (make-cons-node (aref chars 0))))
		    (node-connect-up first-node parent-node)
		    (attach-char-node first-node)
		    (dobox ((index 1 len))
			   (attach-char-node
			    (make-cons-node (aref chars index)))))))
	      (rec (sexp)
		(let ((cdr (cdr sexp))
		      (car (car sexp)))
		  (let ((rightcar (cons (quote car) nil))
			(leftcar (cons (quote cdr) nil)))
		    (let ((cell-car-node (make-cons-node rightcar sexp))
			  (cell-cdr-node (make-cons-node leftcar sexp)))
		      (node-connect-right cell-car-node cell-cdr-node)
		      (let ((old-len counter))
			(if (listp car)
			    (if car
				(progn
				  (attach-char-and-place #\( cell-car-node)
				  (rec car))
				(prin1-and-done nil cell-car-node))
			    (prin1-and-done car cell-car-node))
			(let ((width (1+ (- counter old-len))))
			  (setf (cdr rightcar) width
				(cdr leftcar) width))
			(if (listp cdr)
			    (if cdr
				(progn
				  (attach-char-and-place #\Space cell-cdr-node)
				  (rec cdr))
				(attach-char-and-place #\) cell-cdr-node))
			    (progn ;;;dotted list?
			      (error "fuck you")
			      (princ " . ")
			      (prin1  cdr)
			      (princ ")")))))))))
       (rec form))
     (values start end)))

 (defun setwidth (node width)
   (let ((other (node-left node)))
     (unless other
       (setf other (node-right node)))
     (setf (cdr (car (node-payload other))) width
	   (cdr (car (node-payload node))) width)))

 (defun width-prop (node width)
   (unless (zerop width)
     (when node
       (let ((top (node-up node))) 
	 (let ((payload (node-payload top)))
	   (let ((type (car payload)))
	     (case (car type)
	       (car (let ((cdr-end (jump-cdr node)))
		      (let ((payload (node-payload cdr-end)))
			(let ((newline (cdr payload)))
			  (if (typep newline 'fixnum)
			      (decf (cdr payload) width)
			      (width-prop (node-right cdr-end) width))))))
	       (cdr (setwidth top (+ (cdr type) width))
		    (let ((payload (node-payload node)))
		      (let ((newline (cdr payload)))
			(if (typep newline 'fixnum)
			    (decf (cdr payload) width)
			    (width-prop (node-right node) width)))))
	       (otherwise
		(let ((payload (node-payload node)))
		  (let ((newline (cdr payload)))
		    (if (typep newline 'fixnum)
			(decf (cdr payload) width)
			(width-prop (node-right node) width))))))))))))

 (defparameter *test-tree*
   (copy-tree
    (quote
     (defun print-cells (sexp)
       (let ((cdr (cdr sexp))
	     (car (car sexp)))
	 (if (listp car)
	     (if car
		 (progn
		   (princ "(")
		   (print-cells car))
		 (princ nil))
	     (prin1 car))
	 (if (listp cdr)
	     (if cdr
		 (progn
		   (princ " ")
		   (print-cells cdr))
		 (princ ")"))
	     (progn
	       (princ " . ")
	       (prin1  cdr)
	       (princ ")"))))))))

 (defparameter *node-start* nil)
 (defun reset-test ()
   (setf node (print-cells2 *test-tree*))
   (setf *node-start* (reverse-node (last (reverse-node node))))
   (quote reset-test))


 (defun reload-test ()
   (setf *chunks* (aload "indentation")
	 *node-start* (reverse-node (get-char 0 0 *chunks*))
	 node *node-start*)
   (quote nil)))
