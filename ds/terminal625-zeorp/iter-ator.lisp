(defpackage :iter-ator
  (:use
   #:cl
   #:funland)
  (:nicknames
   #:iateor)
  (:export
   #:iter-ator
   
   #:make-iterator
   #:make-zeroed-iterator
   #:make-default-iterator

   #:p-index
   #:p-array
   #:p-data
   #:p-func
   
   #:pelt
   #:pstep
   #:pread
   #:pemit
   #:with-bound-iterator
   #:with-simply-bound-iterator
   #:bind-iterator-out
   #:bind-iterator-in))

(in-package :iter-ator)

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

(defstruct (iter-ator  (:conc-name p-)
		       (:constructor make-iterator (index array data func)))
  index
  array
  data
  func)

(declaim (ftype (function (&rest t) iter-ator)
		make-default-iterator
		make-zeroed-iterator))
(defun make-default-iterator ()
  (make-iterator 0 nil (load-time-value (vector nil))
		 (lambda (x)
		   (values (1- (array-total-size x)) x))))
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

(declaim (inline pelt (setf pelt)))
(defun pelt (p)
  (aref (p-array p) (p-index p)))
(defun (setf pelt) (value p)
  (setf (aref (p-array p) (p-index p)) value))
(defun pstep (p)
  (with-bound-iterator (next place (array) (index)) p
    (next)))

(defun pread (iterator)
  (pstep iterator)
  (pelt iterator))
(defun pemit (iterator item)
  (pstep iterator)
  (setf (pelt iterator) item))


(defmacro with-simply-bound-iterator ((next place &optional (type t)) iterator &body body)
  (let ((array (gensym))
	(index (gensym))
	(iteratorfoo (gensym)))
    `(let ((,iteratorfoo ,iterator))
       (declare (type iter-ator ,iteratorfoo))
       (iter-ator:with-bound-iterator
	   (,next ,place (,array (or null (simple-array ,type (*)))) (,index))
	   ,iteratorfoo
	 ,@body))))

(defmacro bind-iterator-out ((emit &optional (type t)) iterator &rest body)
  (let ((next (gensym))
	(place (gensym)))
    `(with-simply-bound-iterator (,next ,place ,type) ,iterator
	 (flet ((,emit (value)
		  (,next)
		  (setf ,place value)))
	   ,@body))))

(defmacro bind-iterator-in ((deref &optional (type t)) iterator &rest body)
  (let ((next (gensym))
	(place (gensym)))
    `(with-simply-bound-iterator (,next ,place ,type) ,iterator
	 (flet ((,deref ()
		  (,next)
		  ,place))
	   ,@body))))

#+nil
(progno
;;;;o means output
 (defmacro wasabio ((emit iterator) &body body)
   (let ((next (gensym))
	 (place (gensym)))
     `(with-simply-bound-iterator (,next ,place ,iterator)
	(flet ((,emit (value)
		 (,next)
		 (setf ,place value)))
	  ,@body))))

;;;i means input
 (defmacro wasabii ((deref iterator) &body body)
   (let ((next (gensym))
	 (place (gensym)))
     `(with-simply-bound-iterator (,next ,place ,iterator)
	(flet ((,deref ()
		 (,next)
		 ,place))
	  ,@body))))

 (defmacro wasabiis ((&rest wasabi-pears) &body body)
   (let ((fin (cons 'progn body)))
     (dolist (pear wasabi-pears)
       (setf fin (list fin))
       (push pear fin)
       (push 'wasabii fin))
     fin))

 (defmacro wasabios ((&rest wasabi-pears) &body body)
   (let ((fin (cons 'progn body)))
     (dolist (pear wasabi-pears)
       (setf fin (list fin))
       (push pear fin)
       (push 'wasabio fin))
     fin)))


#+nil
((deftype iter-ator ()
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
