(defpackage :natural-chunks
  (:use :cl
	:fuktard))

(in-package :natural-chunks)

(deftype non-negative-fixnum ()
  (list 'integer 0 most-positive-fixnum))

(defun %ash (n k)
  (declare (type fixnum n)
	   (type (unsigned-byte 8) k)
	   (optimize (speed 3) (safety 0)))
  (the fixnum (ash n k)))

(progn
  (declaim (type (simple-array (or null simple-array) (*)) *scratch-float-array*)
	   (type non-negative-fixnum *scratch-float-array-size*))
  (defparameter *scratch-float-array* (make-array 1 :initial-element nil))
  (defparameter *scratch-float-array-size* (array-total-size *scratch-float-array*)))


(progn
  (declaim (type fixnum +hash-mask+ +index-mask+))
  (defconstant +scratch-float-array-log-size+ 10)
  (defconstant +scratch-float-array-chunk-size+ (expt 2 +scratch-float-array-log-size+))
  (defconstant +hash-mask+ (- +scratch-float-array-chunk-size+ 1))
  (defparameter +index-mask+ (%ash most-positive-fixnum +scratch-float-array-log-size+)))


(defun next-power-of-two (n)
  (expt 2 (1+ (floor (log n 2)))))

(defun create-scratch-float-array-byte ()
  (make-array +scratch-float-array-chunk-size+ :element-type '(unsigned-byte 8)))

(progn
  (declaim (ftype (function () (simple-array single-float (*))) create-scratch-float-array-float))
  (defun create-scratch-float-array-float ()
    (make-array +scratch-float-array-chunk-size+ :element-type 'single-float)))


(progn
  (declaim (inline get-index))
  (declaim (ftype (function (fixnum) fixnum) get-index))
  (locally (declare (optimize (speed 3) (safety 0)))
    
    (defun get-index (n)
      (logand n +hash-mask+)))
  
  (declaim (notinline get-index)))

(progn
  (declaim (inline get-get-index))
  (declaim (ftype (function (fixnum) fixnum) get-get-index))
  (locally (declare (optimize (speed 3) (safety 0)))
    
    (defun get-get-index (n)
      (ash n (- +scratch-float-array-log-size+))))
  
  (declaim (notinline get-get-index)))

(defun resize-array (array new-size initial-element)
  (let ((type (array-element-type array)))
    (let ((newarray (make-array new-size :element-type type :initial-element initial-element)))
      (dotimes (x (min (array-total-size array) new-size))
	(setf (aref newarray x) (aref array x)))
      newarray)))

(defun resize-scratch-float-array (n)
  (setf *scratch-float-array* (resize-array *scratch-float-array* n nil)
	*scratch-float-array-size* n))

(progn
  (declaim (inline get-array-or-nil))
  (declaim (ftype (function ((simple-array t (*)) non-negative-fixnum)
			    (or null (simple-array t (*)))) get-array-or-nil))
  (locally (declare (optimize (speed 3) (safety 0)))    
    (defun get-array-or-nil (array n)
      (aref array n)))
  (declaim (notinline get-array-or-nil)))

(progn
  (declaim (ftype (function (non-negative-fixnum) (values t (member nil t))) get-scratch-float-array))
  (locally (declare (optimize (speed 3) (safety 0))
		    (inline get-index get-get-index get-array-or-nil))
    
    (defun get-scratch-float-array (n)
      (let ((hashcode (get-get-index n)))
	(if (< hashcode *scratch-float-array-size*)
	    (let ((array (get-array-or-nil *scratch-float-array* hashcode)))
	      (if array
		  (let ((chunk-index (get-index n)))
		    (values (aref array chunk-index) t))
		  (values nil nil)))
	    (values nil nil))))))


(progn
  (declaim (ftype (function (fixnum t) (values)) set-scratch-float-array))
  (locally (declare (optimize (speed 3) (safety 0))
		    (inline get-index get-get-index get-array-or-nil))
    (defun set-scratch-float-array (n value)
      (let ((hashcode (get-get-index n))
	    (chunk-index (get-index n)))
	(tagbody
	   (if (< hashcode *scratch-float-array-size*)
	       (let ((array (get-array-or-nil *scratch-float-array* hashcode)))
		 (if array
		     (progn
		       (setf (aref array chunk-index) value)
		       (go end))
		     (go new-chunk)))
	       (progn
		 (resize-scratch-float-array (next-power-of-two hashcode))
		 (go new-chunk)))
	 new-chunk
	   (let ((new-chunk (create-scratch-float-array-float)))
	     (setf (aref new-chunk chunk-index) value)
	     (setf (aref *scratch-float-array* hashcode) new-chunk))
	 end)))))

(progn
  (declaim (ftype (function ((function (single-float)) fixnum fixnum) (values))
		  iterate-scratch-float-array))
  (locally (declare (optimize (speed 3) (safety 0))
		    (inline get-get-index get-index get-array-or-nil))  
    (defun iterate-scratch-float-array (func last-chunk-index offset)
      (dotimes (chunk-index (1+ last-chunk-index))
	(let ((current-chunk-array (get-array-or-nil *scratch-float-array* chunk-index)))
	  (dotimes (index (if (= last-chunk-index chunk-index)
			      offset
			      +scratch-float-array-chunk-size+))
	    (funcall func (aref current-chunk-array index)))))
      (values))))

(defun scratch-float-array-validate (last-chunk-index)
  (if (>= last-chunk-index *scratch-float-array-size*)
      nil
      (dotimes (chunk (1+ last-chunk-index))
	(let ((current-chunk-array (aref *scratch-float-array* chunk)))
	  (unless (and current-chunk-array 
		       (= (array-total-size current-chunk-array) +scratch-float-array-chunk-size+))
	    (return-from scratch-float-array-validate nil)))))
  t)

(defun scratch-float-array-safe-iterate (n func)
  (let ((last-chunk-index (get-get-index n))
	(offset (get-index n)))
    (if (scratch-float-array-validate last-chunk-index)
	(iterate-scratch-float-array func last-chunk-index offset)
	(print "error error"))))
