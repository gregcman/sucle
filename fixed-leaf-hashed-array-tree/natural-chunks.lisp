(defpackage #:fixed-leaf-hashed-array-tree
  (:use #:cl
	#:fuktard
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
