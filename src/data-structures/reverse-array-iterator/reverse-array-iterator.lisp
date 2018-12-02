(defpackage :reverse-array-iterator
  (:use
   #:cl
   #:utility)
  (:export
   #:iterator
   
   #:make-iterator
   #:make-zeroed-iterator
   #:make-default-iterator

   #:p-index
   #:p-array
   #:p-data
   #:p-func
   
   #:with-bound-iterator
   #:with-simply-bound-iterator
   #:bind-iterator-out
   #:bind-iterator-in))

(in-package :reverse-array-iterator)

(defmacro backwards-array-iterator3 (index-var array-var completion-form)
  (let ((new-index (gensym))
	(new-array (gensym))
	(new-index2 (gensym)))
    `(if (zerop ,index-var)
	 (multiple-value-bind (,new-index2 ,new-array) ,completion-form
	   (declare (type fixnum ,new-index2))
	   (setf ,index-var ,new-index2
		 ,array-var ,new-array))
	 (let ((,new-index (1- ,index-var)))
	   (declare (type fixnum ,new-index))
	   (setf ,index-var ,new-index)))))

(defstruct (iterator  (:conc-name p-)
		      (:constructor make-iterator (index array data func)))
  index
  array
  data
  func)

(declaim (ftype (function (&rest t) iterator)
		make-default-iterator
		make-zeroed-iterator))
(defun make-default-iterator ()
  (make-zeroed-iterator   
   (lambda (x)
     (values (1- (array-total-size x)) x))
   (load-time-value (vector nil))))
(defun make-zeroed-iterator (func data)
  (make-iterator 0 nil data func))

(defmacro iterator-transfer (iterator return-type)
  (let ((next-func (gensym)))
    `(let ((,next-func (p-func ,iterator)))
       (declare (type (function (t) ,return-type) ,next-func))
       (funcall ,next-func (p-data ,iterator)))))

(defmacro with-bound-iterator ((next
				place
				(array &optional (array-type '(or null simple-vector)))
				(index &optional (index-type 'fixnum)))
					 iterator &body body)
  `(with-let-mapped-places ((,array (p-array ,iterator))
			    (,index (p-index ,iterator)))
     (declare (type ,array-type ,array)
	      (type ,index-type ,index))
     (flet ((,next ()
	      (backwards-array-iterator3
	       ,index ,array
	       (iterator-transfer ,iterator (values ,index-type ,array-type)))
	      (values)))
       (symbol-macrolet ((,place (aref ,array ,index)))
	 ,@body))))

(defmacro with-simply-bound-iterator ((next place &optional (type t)) iterator &body body)
  (let ((array (gensym))
	(index (gensym))
	(iteratorfoo (gensym)))
    `(let ((,iteratorfoo ,iterator))
       (declare (type iterator ,iteratorfoo))
       (with-bound-iterator
	   (,next ,place (,array (or null (simple-array ,type (*)))) (,index))
	   ,iteratorfoo
	 ,@body))))

(defmacro bind-iterator-out ((emit &optional (type t)) iterator &rest body)
  (let ((next (gensym))
	(place (gensym))
	(actual-emit (gensym)))
    `(with-simply-bound-iterator (,next ,place ,type) ,iterator
       (macrolet ((,emit (&rest forms)
		    (cons 'progn (mapcar (lambda (x) (list (quote ,actual-emit) x))
					 forms))))
	 (flet ((,actual-emit (value)
		  (,next)
		  (setf ,place value)))
	   ,@body)))))

(defmacro bind-iterator-in ((deref &optional (type t)) iterator &rest body)
  (let ((next (gensym))
	(place (gensym)))
    `(with-simply-bound-iterator (,next ,place ,type) ,iterator
	 (flet ((,deref ()
		  (,next)
		  ,place))
	   ,@body))))

#+nil
((deftype iterator ()
   `(simple-vector 4))
 (progn
   (declaim (inline p-index (setf p-index)
		    p-array (setf p-array)
		    p-data (setf p-data)
		    p-func (setf p-func)))
   (defun p-index (p)
     (aref p 0))
   (defun (setf p-index) (n p)
     (setf (aref p 0) n))
   (defun p-array (p)
     (aref p 1))
   (defun (setf p-array) (n p)
     (setf (aref p 1) n))
   (defun p-data (p)
     (aref p 2))
   (defun (setf p-data) (n p)
     (setf (aref p 2) n))
   (defun p-func (p)
     (aref p 3))
   (defun (setf p-func) (n p)
     (setf (aref p 3) n))))
