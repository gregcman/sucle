(defpackage :iter-ator
  (:use
   #:cl
   #:foo-mapped-bar)
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
   #:with-bound-iterator))

(in-package :iter-ator)

(defmacro decf-unless-zerop (index-place otherwise-form)
  (let ((new-index (gensym)))
    `(if (zerop ,index-place)
	 ,otherwise-form
	 (let ((,new-index (1- ,index-place)))
	   (declare (type fixnum ,new-index))
	   (setf ,index-place ,new-index)))))

(defmacro backwards-array-iterator3 (index-var array-var completion-form)
  (let ((new-index (gensym))
	(new-array (gensym)))
    `(decf-unless-zerop
      ,index-var
      (multiple-value-bind (,new-index ,new-array) ,completion-form
	(declare (type fixnum ,new-index))
	(setf ,index-var ,new-index
	      ,array-var ,new-array)))))

(deftype iter-ator ()
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
    (setf (aref p 3) n)))

(defun basic-loop-func (x)
  (values (1- (array-total-size x)) x))


(declaim (ftype (function (&rest t) iter-ator)
		make-default-iterator
		make-iterator
		make-zeroed-iterator))
(defun make-default-iterator ()
  (vector 0 nil (load-time-value (vector nil)) (function basic-loop-func)))
(defun make-iterator (start current data func) 
  (vector start current data func))
(defun make-zeroed-iterator (func data)
  (vector 0 nil data func))

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
  `(with-let-mapped-places ((,array (p-array ,iterator) ,array-type)
			    (,index (p-index ,iterator) ,index-type))
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
