(in-package :flhat)

(defun testwtf ()
  (declare (optimize (speed 3) (safety 0)))
  (dotimes (x 25)
    (let ((bar (make-flhat-pointer *flhat*)))
      (dotimes (x (expt 10 7))
	(next-index bar)))))

(defparameter *flhat* (make-flhat))

(defun testwtf2 () 
  (declare (optimize (speed 3) (safety 0)))
  (dotimes (x (if nil 1 25))
    (with-flhat-iterator (next place *flhat*)
      (dotimes (x (expt 10 (if nil 6 7)))
	(next)
	(setf place x))))
  (values))

(progn
  (declaim (inline offset-index))
  (declaim (ftype (function (fixnum) fixnum) offset-index))
  (locally (declare (optimize (speed 3) (safety 0)))
    
    (defun offset-index (n)
      (logandc1 n +hash-mask+)))
  
  (declaim (notinline offset-index)))

(progn
  (declaim (inline chunk-index))
  (declaim (ftype (function (fixnum) fixnum) chunk-index))
  (locally (declare (optimize (speed 3) (safety 0)))
    
    (defun chunk-index (n)
      (ash n (- +log-size+))))
  
  (declaim (notinline chunk-index)))

(progn
  (declaim (inline get-array-or-nil))
  (declaim (ftype (function (simple-vector fixnum)
			    (or null simple-vector)) get-array-or-nil))
  (locally (declare (optimize (speed 3) (safety 0)))    
    (defun get-array-or-nil (array n)
      (aref array n)))
  (declaim (notinline get-array-or-nil)))

(progn
  (declaim (ftype (function (cons fixnum) (or t null)) ref))
  (locally (declare (optimize (speed 3) (safety 0))
		    (inline offset-index
			    chunk-index
			    get-array-or-nil
			    array-array-array
			    array-array-length))
    
    (defun ref (array-array n)
      (let ((array-length (array-array-length array-array))
	    (hashcode (chunk-index n)))
	(if (< hashcode array-length)
	    (let ((array (array-array-array array-array)))
	      (let ((sub-array (get-array-or-nil array hashcode)))
		(if sub-array
		    (let ((chunk-index (offset-index n)))
		      (values (aref sub-array chunk-index) t))))))))))

(progn
  (declaim (ftype (function (cons fixnum t))
		  sets))
  (declaim (inline sets))
  (locally (declare (optimize (speed 3) (safety 0))
		    (inline offset-index
			    chunk-index
			    get-array-or-nil
			    array-array-array
			    array-array-length))
    (defun sets (array-array n value)
      (let ((hashcode (chunk-index n))
	    (array (array-array-array array-array)))
	(let ((array-length (array-array-length array-array)))
	  (unless (< hashcode array-length)
	    (setf (values array-length array) (fit-resize array-array hashcode))))
	(let ((sub-array (get-array-or-nil array hashcode)))
		(if sub-array
		    (setf (aref sub-array (offset-index n)) value)
		    (let ((new-array (create-scratch-array)))
		      (setf (aref array hashcode) new-array)
		      (setf (aref new-array (offset-index n)) value)))))))
  (declaim (notinline sets)))

(defun (setf ref) (value array-array n)
  (sets array-array n value))


(defun validate (array array-length last-chunk-index)
  (if (>= last-chunk-index array-length)
      nil
      (dotimes (chunk (1+ last-chunk-index))
	(let ((current-chunk-array (aref array chunk)))
	  (unless (and current-chunk-array 
		       (= (array-total-size current-chunk-array) +size+))
	    (return-from validate nil)))))
  t)

(defun safe-iterate (array array-length n func)
  (let ((last-chunk-index (chunk-index n))
	(offset (offset-index n)))
    (if (validate array array-length last-chunk-index)
	(iterates array func last-chunk-index offset)
	(error 'simple-error))))

(defun iterate-array-array (array-array func times)
  (safe-iterate (array-array-array array-array)
		(array-array-length array-array)
		times
		func))

(progn
  (declaim (ftype (function (simple-vector (function (t)) fixnum fixnum))
		  iterates))
  (locally (declare (optimize (speed 3) (safety 0))
		    (inline chunk-index offset-index get-array-or-nil))  
    (defun iterates (array func last-chunk-index offset)
      (dotimes (chunk-index (1+ last-chunk-index))
	(let ((current-chunk-array (get-array-or-nil array chunk-index)))
	  (dotimes (index (if (= last-chunk-index chunk-index)
			      offset
			      +size+))
	    (funcall func (aref current-chunk-array index))))))))

(progn
  (defun resize-array (array new-size)
    (let ((old-size (array-total-size array)))
      (if (= old-size new-size)
	  array
	  (let ((newarray (make-array new-size :element-type t :initial-element nil)))
	    (dotimes (x (min old-size new-size))
	      (setf (aref newarray x) (aref array x)))
	    newarray))))

  (progn
    (declaim (ftype (function (cons fixnum)
			      (values fixnum simple-vector))
		    resize-array-array))
    (defun resize-array-array (array-array new-size)
      (values (setf (array-array-length array-array) new-size)
	      (setf (array-array-array array-array)
		    (resize-array (array-array-array array-array) new-size)))))

  (progn
    (declaim (ftype (function (cons fixnum)
			      (values fixnum simple-vector))
		    fit-resize))
    (defun fit-resize (array-array leaf-capacity)
      (let ((new-size (next-power-of-two leaf-capacity)))
	(resize-array-array array-array new-size))))
  (progn
    (declaim (ftype (function (cons fixnum fixnum)
			      (function (t))) make-setter))
    (locally (declare (optimize (speed 3) (safety 0)))
      (defun make-setter (array-array index chunk-index)
	(declare (type fixnum index chunk-index))
	(let ((array (array-array-array array-array))
	      (array-length (array-array-length array-array)))
	  (let ((sub-array (aref array chunk-index)))
	    (declare (type (or null simple-vector) sub-array))
	    (unless sub-array
	      (setf sub-array (create-scratch-array))
	      (setf (aref array chunk-index) sub-array))
	    (labels ((end-of-chunk ()
		       (setf index 0)
		       (progn
			 (let ((next (1+ chunk-index)))
			   (declare (type fixnum next))
			   (setf chunk-index next)
			   (when (= next array-length)
			     (setf (values array-length array)
				   (fit-resize array-array next))))
			 (let ((next-array (aref array chunk-index)))
			   (declare (type (or null simple-vector) next-array))
			   (if next-array
			       (setf sub-array next-array)
			       (let ((newarray (create-scratch-array)))
				 (setf sub-array newarray)
				 (setf (aref array chunk-index) newarray))))))
		     (func (x)
		       (setf (svref sub-array index) x)
		       (incf index)
		       (when (= index +size+)
			 (end-of-chunk))))
	      #'func)))))))
(progn
  (declaim (ftype (function (cons fixnum fixnum)) %make-setter))
  (locally (declare (optimize (speed 3) (safety 0)))
    (defun %make-setter (array-array index chunk-index)
      (declare (type fixnum index chunk-index))
      (let ((array (array-array-array array-array))
	    (array-length (array-array-length array-array)))
	(let ((sub-array (aref array chunk-index)))
	  (declare (type (or null simple-vector) sub-array))
	  (unless sub-array
	    (setf sub-array (create-scratch-array))
	    (setf (aref array chunk-index) sub-array))
	  (labels ((func (x)
		     (setf (aref sub-array index) x)
		     (incf index)
		     (when (= index +size+)
		       (setf index 0)
		       (progn
			 (let ((next (1+ chunk-index)))
			   (declare (type fixnum next))
			   (setf chunk-index next)
			   (when (= next array-length)
			     (setf (values array-length array)
				   (fit-resize array-array next))))
			 (let ((next-array (aref array chunk-index)))
			   (declare (type (or null simple-vector) next-array))
			   (if next-array
			       (setf sub-array next-array)
			       (let ((newarray (create-scratch-array)))
				 (setf sub-array newarray)
				 (setf (aref array chunk-index) newarray))))))))
	    (dotimes (x (expt 10 6))
	      (func x))))))))


(defparameter *scratch* (make-array-array 1))

(defun testwtf ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((counter (make-setter *scratch* 0 0))) 
    (dotimes (x (expt 10 6))
      (funcall counter x))))

(defun testwtf? ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((counter (constantly nil))) 
    (dotimes (x (expt 10 6))
      (funcall counter 0))))

(defun testwtf2 ()
  (declare (optimize (speed 3) (safety 0)))
  (%make-setter *scratch* 0 0) )

(progn
  (declaim (ftype (function (cons fixnum fixnum)
			    (function (t))) make-setter))
  (locally (declare (optimize (speed 3) (safety 0)))
    (defun make-setter3 (array-array index chunk-index)
      (declare (type fixnum index chunk-index))
      (let ((array (array-array-array array-array))
	    (array-length (array-array-length array-array)))
	(let ((sub-array (aref array chunk-index)))
	  (declare (type (or null simple-vector) sub-array))
	  (unless sub-array
	    (setf sub-array (create-scratch-array))
	    (setf (aref array chunk-index) sub-array))
	  (labels ((end-of-chunk ()
		     (setf index +size+)
		     (progn
		       (let ((next (1+ chunk-index)))
			 (declare (type fixnum next))
			 (setf chunk-index next)
			 (when (= next array-length)
			   (setf (values array-length array)
				 (fit-resize array-array next))))
		       (let ((next-array (aref array chunk-index)))
			 (declare (type (or null simple-vector) next-array))
			 (if next-array
			     (setf sub-array next-array)
			     (let ((newarray (create-scratch-array)))
			       (setf sub-array newarray)
			       (setf (aref array chunk-index) newarray))))))
		   (func (x)
		     (decf index)
		     (setf (aref sub-array index) x)
		     (when (zerop index)
		       (end-of-chunk))))
	    #'func))))))

(defun testwtf3 ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((counter (make-setter3 *scratch* 0 0))) 
    (dotimes (x (expt 10 6))
      (funcall counter x))))

(defun wot (n)
  (declare (type fixnum n)
	   (optimize (speed 3) (safety 0)))
  (if t
      (= 1024 n)
      (zerop n))) 

(progn  (progn
	  (declaim (ftype (function (simple-vector fixnum t))
			  %sets))
	  (declaim (inline %sets))
	  (locally (declare (optimize (speed 3) (safety 0))
			    (inline offset-index chunk-index get-array-or-nil))
	    (defun %sets (array n value)
	      (let ((hashcode (chunk-index n)))
		(let ((sub-array (get-array-or-nil array hashcode)))
		  (if sub-array
		      (setf (aref sub-array (offset-index n)) value)
		      (let ((new-array (create-scratch-array)))
			(setf (aref new-array (offset-index n)) value)
			(setf (aref array hashcode) new-array)))))))
	  (declaim (notinline %sets)))

	(progn
	  (declaim (ftype (function (simple-vector fixnum) (or t null)) %gets))
	  (locally (declare (optimize (speed 3) (safety 0))
			    (inline offset-index chunk-index get-array-or-nil))
	    
	    (defun %gets (array n)
	      (let ((hashcode (chunk-index n)))
		(let ((sub-array (get-array-or-nil array hashcode)))
		  (if array
		      (let ((chunk-index (offset-index n)))
			(aref sub-array chunk-index)))))))))

(defmacro backwards-array-iterator (index-var array-var completion-form)
  `(if (zerop ,index-var)
       (setf (values ,index-var ,array-var)
	     ,completion-form)
       (decf ,index-var)))

(defun %make-iterator (array-array)
  (declare (optimize (speed 3) (safety 0))
	   (inline array-array-array array-array-length))
  (let ((array nil)
	(sub-array nil)
	(meta-index 0)
	(chunk-index 0)
	(index 0))
    (declare (type (or null cons) array-array)
	     (type (or null simple-vector) array sub-array)
	     (type fixnum meta-index chunk-index index))
    (labels ((func (x)
	       (backwards-array-iterator index sub-array (next-sub-array))
	       
	       (setf (aref sub-array index) x))
	     
	     (next-sub-array ()
	       (backwards-array-iterator chunk-index array (next-array-array))
	       
	       (values 
		(1- +size+)
		(let ((next-array (aref array chunk-index)))
		  (declare (type (or null simple-vector) next-array))
		  (if next-array
		      next-array
		      (let ((newarray (create-scratch-array)))
			(setf (aref array chunk-index) newarray)
			newarray)))))
	     
	     (next-array-array ()
	       (backwards-array-iterator meta-index array-array (init))

	       (if (= -1 meta-index)
		   (let ((size (1- (array-array-length array-array))))
		     (declare (type fixnum size))
		     (values size (array-array-array array-array)))
		   (let ((old-size (array-array-length array-array)))
		     (declare (type fixnum old-size))
		     (multiple-value-bind (size data)
			 (reverse-fit-resize array-array old-size)
		       (declare (type fixnum size))
		       (let ((size (1- (- size old-size))))
			 (declare (type fixnum size))
			 (values size data))))))
	     
	     (init ()
	       (values -1 array-array)))
      (declare (ftype (function (t) (values)) func)
	       (ftype (function () (values fixnum simple-vector)) next-sub-array)
	       (ftype (function () (values fixnum simple-vector)) next-array-array)
	       (ftype (function () (values fixnum cons)) init))
      (if nil
	  #'func
	  (dotimes (x (expt 10 6))
	    (func x))))))

(defmacro backwards-array-iterator (index-var array-var completion-form)
  `(if (zerop ,index-var)
       (setf (values ,index-var ,array-var)
	     ,completion-form)
       (decf ,index-var)))

(defun %make-iterator2 (flhat)
  
  (declare (optimize (speed 3) (safety 0))
	   (inline array-array-array array-array-length))
  
  (labels ((init ()
	     (values -1 flhat)))
    (declare (ftype (function () (values fixnum cons)) init))

    (let ((array nil)
	  (meta-index 0)
	  (array-array nil))
      (declare (type (or null simple-vector) array)
	       (type fixnum meta-index)
	       (type (or null cons) array-array)) 
      (labels ((next-array-array ()
		 (backwards-array-iterator meta-index array-array (init))

		 (if (= -1 meta-index)
		     (let ((size (1- (array-array-length array-array))))
		       (declare (type fixnum size))
		       (values size (array-array-array array-array)))
		     (let ((old-size (array-array-length array-array)))
		       (declare (type fixnum old-size))
		       (multiple-value-bind (size data)
			   (reverse-fit-resize array-array old-size)
			 (declare (type fixnum size))
			 (let ((size (1- (- size old-size))))
			   (declare (type fixnum size))
			   (values size data)))))))
	(declare (ftype (function () (values fixnum simple-vector)) next-array-array))

	(let ((chunk-index 0))
	  (declare (type fixnum chunk-index))
	  (labels ((next-sub-array ()
		     (backwards-array-iterator chunk-index array (next-array-array))	       
		     (values 
		      (1- +size+)
		      (let ((next-array (aref array chunk-index)))
			(declare (type (or null simple-vector) next-array))
			(if next-array
			    next-array
			    (let ((newarray (create-scratch-array)))
			      (setf (aref array chunk-index) newarray)
			      newarray))))))
	    (declare (ftype (function () (values fixnum simple-vector)) next-sub-array))
	    
	    (let ((sub-array nil)
		  (index 0))
	      (declare (type (or null simple-vector) sub-array)
		       (type fixnum index))
	      (labels ((func (x)
			 (backwards-array-iterator index sub-array (next-sub-array))	       
			 (setf (aref sub-array index) x)))
		(declare (ftype (function (t) (values)) func))	  
		(if nil
		    #'func
		    (dotimes (x (expt 10 6))
		      (func x)))))))))))

