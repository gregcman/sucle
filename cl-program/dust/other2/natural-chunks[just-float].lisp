(defpackage :natural-chunks
  (:use :cl))

(in-package :natural-chunks)

(defun next-power-of-two (n)
  (ash 2 (floor (log n 2))))

(defun resize-array (array new-size initial-element)
  (let ((type (array-element-type array)))
    (let ((newarray (make-array new-size :element-type type :initial-element initial-element)))
      (dotimes (x (min (array-total-size array) new-size))
	(setf (aref newarray x) (aref array x)))
      newarray)))

(progn
  (declaim (type fixnum +hash-mask+ +index-mask+))
  (defconstant +log-size+ 10)
  (defconstant +size+ (expt 2 +log-size+))
  (defconstant +hash-mask+ (- +size+ 1))
  (defconstant +index-mask+ (logior most-positive-fixnum +hash-mask+)))

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

(defun fit-resize (array n)
  (let ((newsize (next-power-of-two (chunk-index n))))
    (values (resize-array array newsize nil) n)))

(progn
  (declaim (ftype (function () (simple-array single-float (*))) create-scratch-float-array-float))
  (defun create-scratch-float-array-float ()
    (make-array +size+ :element-type 'single-float)))

(progn
  (declaim (inline get-float-array-or-nil))
  (declaim (ftype (function ((simple-array t (*)) fixnum)
			    (or null (simple-array single-float (*)))) get-float-array-or-nil))
  (locally (declare (optimize (speed 3) (safety 0)))    
    (defun get-float-array-or-nil (array n)
      (aref array n)))
  (declaim (notinline get-float-array-or-nil)))

(progn
  (declaim (ftype (function (simple-vector fixnum fixnum) (or t null)) get-single-float))
  (locally (declare (optimize (speed 3) (safety 0))
		    (inline offset-index chunk-index get-float-array-or-nil))
    
    (defun get-single-float (array array-length n)
      (let ((hashcode (chunk-index n)))
	(if (< hashcode array-length)
	    (let ((sub-array (get-float-array-or-nil array hashcode)))
	      (if array
		  (let ((chunk-index (offset-index n)))
		    (aref sub-array chunk-index)))))))))

(progn
  (declaim (ftype (function (simple-vector fixnum fixnum t))
		  set-single-float))
  (locally (declare (optimize (speed 3) (safety 0))
		    (inline offset-index chunk-index get-float-array-or-nil))
    (defun set-single-float (array array-length n value)
      (let ((hashcode (chunk-index n)))
	(if (< hashcode array-length)
	    (let ((sub-array (get-float-array-or-nil array hashcode)))
	      (if sub-array
		  (setf (aref sub-array (offset-index n)) value)
		  (let ((new-array (create-scratch-float-array-float)))
		    (setf (aref new-array (offset-index n)) value)
		    (setf (aref array hashcode) new-array)))))))))

(progn
  (declaim (ftype (function (simple-vector (function (single-float)) fixnum fixnum))
		  iterate-single-float))
  (locally (declare (optimize (speed 3) (safety 0))
		    (inline chunk-index offset-index get-float-array-or-nil))  
    (defun iterate-single-float (array func last-chunk-index offset)
      (dotimes (chunk-index (1+ last-chunk-index))
	(let ((current-chunk-array (get-float-array-or-nil array chunk-index)))
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
	(iterate-single-float array func last-chunk-index offset)
	(print "error error"))))

(progn
  (declaim (ftype (function (simple-vector fixnum t))
		  %set-single-float))
  (declaim (inline %set-single-float))
  (locally (declare (optimize (speed 3) (safety 0))
		    (inline offset-index chunk-index get-float-array-or-nil))
    (defun %set-single-float (array n value)
      (let ((hashcode (chunk-index n)))
	(let ((sub-array (get-float-array-or-nil array hashcode)))
	  (if sub-array
	      (setf (aref sub-array (offset-index n)) value)
	      (let ((new-array (create-scratch-float-array-float)))
		(setf (aref new-array (offset-index n)) value)
		(setf (aref array hashcode) new-array)))))))
  (declaim (notinline %set-single-float)))

(progn
  (declaim (ftype (function (simple-vector fixnum) (or t null)) %get-single-float))
  (locally (declare (optimize (speed 3) (safety 0))
		    (inline offset-index chunk-index get-float-array-or-nil))
    
    (defun %get-single-float (array n)
      (let ((hashcode (chunk-index n)))
	(let ((sub-array (get-float-array-or-nil array hashcode)))
	  (if array
	      (let ((chunk-index (offset-index n)))
		(aref sub-array chunk-index))))))))


(progn
  (declaim (ftype (function (simple-vector fixnum fixnum)) make-setter))
  (locally (declare (optimize (speed 3) (safety 0))
		    (inline %set-single-float))
    (defun make-setter (array index chunk-index)
      (declare (type fixnum index chunk-index))
      (let ((sub-array (aref array chunk-index))
	    (array-length (array-total-size array)))
	(declare (type (or null (simple-array single-float (*))) sub-array)
		 (type fixnum array-length))
	(unless sub-array
	  (setf sub-array (create-scratch-float-array-float))
	  (setf (aref array chunk-index) sub-array))
	(labels ((func (x)
		   (setf (aref sub-array index) x)
		   (setf index (1+ index))
		   (when (= index +size+)
		     (setf index 0)
		     (let ((next (1+ chunk-index)))
		       (if (= next array-length)
			   (error 'simple-error)
			   (setf chunk-index next)))
		     (let ((next-array (aref array chunk-index)))
		       (declare (type (or null (simple-array single-float (*))) next-array))
		       (if next-array
			   (setf sub-array next-array)
			   (let ((newarray (create-scratch-float-array-float)))
			     (setf sub-array newarray)
			     (setf (aref array chunk-index) newarray)))))
		   (values)))
	  #'func)))))

(progn
  (declaim (ftype (function (simple-vector fixnum fixnum)) %make-setter))
  (locally (declare (optimize (speed 3) (safety 0))
		    (inline %set-single-float))
    (defun %make-setter (array index chunk-index)
      (declare (type fixnum index chunk-index))
      (let ((sub-array (aref array chunk-index))
	    (array-length (array-total-size array)))
	(declare (type (or null (simple-array single-float (*))) sub-array)
		 (type fixnum array-length))
	(unless sub-array
	  (setf sub-array (create-scratch-float-array-float))
	  (setf (aref array chunk-index) sub-array))

	
	(dotimes (x (expt 10 6))
	  (setf (aref sub-array index) (float x))
	  (setf index (1+ index))
	  (when (= index +size+)
	    (setf index 0)
	    (let ((next (1+ chunk-index)))
	      (if (= next array-length)
		  (error 'simple-error)
		  (setf chunk-index next)))
	    (let ((next-array (aref array chunk-index)))
	      (declare (type (or null (simple-array single-float (*))) next-array))
	      (if next-array
		  (setf sub-array next-array)
		  (let ((newarray (create-scratch-float-array-float)))
		    (setf sub-array newarray)
		    (setf (aref array chunk-index) newarray))))))))))
(defun create-scratch-float-array-byte ()
  (make-array +size+ :element-type '(unsigned-byte 8)))

(defparameter *scratch* (make-array 1024 :element-type t :initial-element nil))
(defparameter *size* (array-total-size *scratch*))
