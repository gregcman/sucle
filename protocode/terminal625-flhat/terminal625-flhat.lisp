(defpackage #:fixed-leaf-hashed-array-tree
  (:use #:cl
	#:funland
	#:iter-ator)
  (:nicknames #:flhat)
  (:export
   #:flhat
   #:make-flhat ;;;;make the flhat
   #:flhat-length
   #:flhat-data
   #:reverse-fit-resize
   
   #:xelt

   #:reset-iterator
   #:iterator-position
   #:relocate-iterator
   #:make-flhat-iterator
   #:with-flhat-iterator))

(in-package :fixed-leaf-hashed-array-tree)

(deftype flhat ()
  `(cons fixnum (or null vector)))

(defmacro define-construct
    ((name-one &optional (type-one t)) (name-two &optional (type-two t)))
  `(progn
    (declaim (inline ,name-one ,name-two (setf ,name-one) (setf ,name-two))
	     (ftype (function (cons) ,type-one) ,name-one)
	     (ftype (function (,type-one cons) ,type-one) (setf ,name-one))
	     (ftype (function (cons) ,type-two) ,name-two)
	     (ftype (function (,type-two cons) ,type-two) (setf ,name-two)))
    (defun ,name-one (construct)
      (car construct))
    (defun (setf ,name-one) (value construct)
      (setf (car construct) value))
    (defun ,name-two (construct)
      (cdr construct))
    (defun (setf ,name-two) (value construct)
      (setf (cdr construct) value))))

(defun next-power-of-two (n)
  (ash 2 (floor (log (max 1 n) 2))))

(defun reverse-resize-array (array new-size)
  (let ((old-size (array-total-size array)))
    (if (= old-size new-size)
	array
	(let ((type (array-element-type array)))
	  (let ((newarray (make-array new-size :element-type type :initial-element nil)))
	    (let ((old-offset (1- old-size))
		  (new-offset (1- new-size)))
	      (dotimes (x (min old-size new-size))
		(setf (aref newarray (- new-offset x))
		      (aref array (- old-offset x)))))
	    newarray)))))

(define-construct
    (flhat-length fixnum)
    (flhat-data simple-vector))

(progn
  (declaim (type fixnum +hash-mask+ +index-mask+))
  (defconstant +log-size+ 10)
  (defconstant +size+ (expt 2 +log-size+)))

(progn
  (declaim (ftype (function () simple-vector) create-scratch-array))
  (defun create-scratch-array ()
    (make-array +size+ :element-type t)))

(defun make-flhat (&optional (length 1))
  (let ((top (make-array length :element-type t :initial-element nil)))
    (cons length top)))

(defun xindex (flhat n)
  (let ((size (flhat-length flhat)))
    (let ((not (lognot n)))
      (let ((offset-index (logand not (- +size+ 1)))
	    (chunk-index (logand (ash not (- +log-size+)) (1- size))))
	(values chunk-index offset-index)))))

(defun xelt (flhat n)
  (multiple-value-bind (chunk-index offset-index) (xindex flhat n)
    (let ((sub-array (aref (flhat-data flhat) chunk-index)))
      (if sub-array
	  (values (aref sub-array offset-index) t)))))

(defun (setf xelt) (value flhat n)
  (multiple-value-bind (chunk-index offset-index) (xindex flhat n)
    (let ((data (flhat-data flhat)))
      (let ((sub-array (aref data chunk-index)))
	(if sub-array
	    (setf (aref sub-array offset-index) value)
	    (let ((new-array (create-scratch-array)))
	      (setf (aref data chunk-index) new-array)
	      (setf (aref new-array offset-index) value)))))))

(progn
  (declaim (ftype (function (flhat fixnum)
			    (values fixnum simple-vector))
		  reverse-resize-flhat))
  
  (defun reverse-resize-flhat (flhat new-size)
    (values (setf (flhat-length flhat) new-size)
	    (setf (flhat-data flhat)
		  (reverse-resize-array (flhat-data flhat) new-size)))))
(progn
  (declaim (ftype (function (flhat fixnum)
			    (values fixnum simple-vector))
		  fit-resize))
  (defun reverse-fit-resize (flhat leaf-capacity)
    (let ((new-size (next-power-of-two leaf-capacity)))
      (reverse-resize-flhat flhat new-size))))


(in-package :fixed-leaf-hashed-array-tree)

(declaim (ftype (function (iter-ator))
		next-index)
	 (ftype (function (iter-ator)
			  (values fixnum simple-vector))
		next-subarray)
	 (ftype (function (iter-ator)
			  (values fixnum simple-vector))
		next-flhat))

(with-unsafe-speed
  (defun next-init (p)
    (values -1 p)))

(with-unsafe-speed
  (defun next-flhat (p)
    (with-bound-iterator (next place (flhat flhat) (meta-index)) p
      (next)
      
      (if (= -1 meta-index)
	  (let ((size (1- (flhat-length flhat))))
	    (declare (type fixnum size))
	    (values size (flhat-data flhat)))
	  (let ((old-size (flhat-length flhat)))
	    (declare (type fixnum old-size))
	    (multiple-value-bind (size data) (reverse-fit-resize flhat old-size)
	      (declare (type fixnum size))
	      (let ((size (1- (- size old-size))))
		(declare (type fixnum size))
		(values size data))))))))

(with-unsafe-speed
  (defun next-index (p)
    (with-bound-iterator (next place (array) (index)) p
      (next))))

(with-unsafe-speed
  (defun next-subarray (p)
    (with-bound-iterator (next place (array) (chunk-index)) p
      (next)
      
      (values 
       (1- +size+)
       (let ((next-array place))
	 (if next-array
	     next-array
	     (let ((new (create-scratch-array)))
	       (setf place new))))))))

(progn
  (declaim (ftype (function (flhat) iter-ator) make-flhat-iterator))
  (defun make-flhat-iterator (flhat)
    (let* ((flhat-iter (make-zeroed-iterator #'next-init flhat))
	   (array-iter (make-zeroed-iterator #'next-flhat flhat-iter))
	   (value-iter (make-zeroed-iterator #'next-subarray array-iter)))
      value-iter)))


(defmacro with-flhat-iterator ((name value-place flhat) &body body)
  (let ((iterator (gensym))
	(array (gensym))
	(index (gensym)))
    `(let ((,iterator (make-flhat-iterator ,flhat)))
       (with-bound-iterator (,name ,value-place (,array) (,index)) ,iterator
	 ,@body))))

(defun relocate-iterator (value-iter n)
  (let* ((array-iter (p-data value-iter))
	 (flhat-iter (p-data array-iter))
	 (flhat (p-data flhat-iter)))
    (multiple-value-bind (chunk-index offset-index) (xindex flhat n)
      (setf (p-index array-iter) chunk-index
	    (p-index value-iter) offset-index
	    (p-array flhat-iter) flhat
	    (p-index flhat-iter) -1)
      (let ((array (flhat-data flhat)))
	(setf (p-array array-iter) array)
	(setf (p-array value-iter)
	      (let ((sub-array (aref array chunk-index)))
		(if sub-array
		    sub-array
		    (let ((new-array (create-scratch-array)))
		      (setf (aref array chunk-index)
			    new-array)))))))
    value-iter))

(defun reset-iterator (value-iter)
  (let* ((array-iter (p-data value-iter))
	 (flhat-iter (p-data array-iter)))
    (setf (p-index value-iter) 0
	  (p-array value-iter) nil
	  (p-index array-iter) 0
	  (p-array array-iter) nil
	  (p-index flhat-iter) 0
	  (p-array flhat-iter) nil)
    value-iter))

(defun iterator-position (value-iter)
  (let* ((array-iter (p-data value-iter))
	 (flhat-iter (p-data array-iter))
	 (flhat (p-data flhat-iter)))
    (let ((value-index (p-index value-iter))
	  (array-index (p-index array-iter))
	  (total-size (flhat-length flhat)))
      (+ (ash (- total-size array-index 1) +log-size+)
	 (- +size+ value-index 1)))))
