(defpackage #:fixed-leaf-hashed-array-tree
  (:use #:cl)
  (:nicknames #:flhat)
  (:export
   #:make-array-array ;;;;make the flhat
   #:sets ;;;;set an arbitrary positive integer index
   #:ref ;;;;get and arbitray nonnegative integer index
   #:iterate-array-array ;;;iterate from zero to a number with a function 
   #:make-setter;;;funcall this with a single object to set it, then increment
   #:array-array-length
   #:array-array-array
   #:offset-index ;;;index of the element in subarray
   #:chunk-index ;;;;the index of the subarray
   #:fit-resize ;;;resize flhat to the next power of two that fits the leaf amount
   ))

(in-package :fixed-leaf-hashed-array-tree)

(defun next-power-of-two (n)
  (ash 2 (floor (log (max 1 n) 2))))

(progn
  (declaim (type fixnum +hash-mask+ +index-mask+))
  (defconstant +log-size+ 10)
  (defconstant +size+ (expt 2 +log-size+))
  (defconstant +hash-mask+ (- +size+ 1))
  (defconstant +index-mask+ (logior most-positive-fixnum +hash-mask+)))
(progn
  (declaim (inline array-array-length array-array-array))
  (declaim (ftype (function (cons) fixnum) array-array-length))
  (defun array-array-length (array-array)
    (car array-array))
  (defun (setf array-array-length) (n array-array)
    (setf (car array-array) n))
  (declaim (ftype (function (cons) simple-vector) array-array-array))
  (defun array-array-array (array-array)
    (cdr array-array))
  (defun (setf array-array-array) (n array-array)
    (setf (cdr array-array) n))
  (declaim (notinline array-array-length array-array-array)))

(progn
  (declaim (inline offset-index))
  (declaim (ftype (function (fixnum) fixnum) offset-index))
  (locally (declare (optimize (speed 3) (safety 0)))
    
    (defun offset-index (n)
      (logand n +hash-mask+)))
  
  (declaim (notinline offset-index)))

(progn
  (declaim (inline chunk-index))
  (declaim (ftype (function (fixnum) fixnum) chunk-index))
  (locally (declare (optimize (speed 3) (safety 0)))
    
    (defun chunk-index (n)
      (ash n (- +log-size+))))
  
  (declaim (notinline chunk-index)))

(progn
  (declaim (ftype (function () simple-vector) create-scratch-array))
  (defun create-scratch-array ()
    (make-array +size+ :element-type t)))

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
	    #'func))))))


(defun make-array-array (&optional (length 1))
  (let ((top (make-array length :element-type t :initial-element nil)))
    (cons length top)))


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

