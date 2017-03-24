(in-package :fixed-leaf-hashed-array-tree)

(defun type-multimap-alist (type varname alist)
  (let ((value (assoc type alist)))
    (if value
	(push varname (cdr value))
	(push (list type varname) alist))
    alist))

(defmacro with-load-unload ((&rest place-pairs) &body body)
  (let ((let-args nil)
	(setf-args nil)
	(type-multimap-alist nil))
    (dolist (place-pair place-pairs)
      (let ((reg-place (pop place-pair))
	    (ram-place (pop place-pair))
	    (type (pop place-pair)))
	(push (list reg-place ram-place) let-args)
	(push reg-place setf-args)
	(push ram-place setf-args)
	(if type
	    (setf type-multimap-alist (type-multimap-alist type reg-place type-multimap-alist)))))
    `(let ,let-args
       ,(cons 'declare
	      (mapcar (lambda (x)
			(cons 'type x))
		      type-multimap-alist))
       (multiple-value-prog1
	   ,(cons 'progn body)
	 ,(cons 'setf setf-args)))))

(defmacro decf-unless-zerop (index-place otherwise-form)
  (let ((new-index (gensym)))
    `(if (zerop ,index-place)
	 ,otherwise-form
	 (let ((,new-index (1- ,index-place)))
	   (declare (type fixnum ,new-index))
	   (setf ,index-place ,new-index)))))

(progn
  (declaim (inline p-index (setf p-index)
		   p-current-array (setf p-current-array)
		   p-data (setf p-data)))
  (defun p-index (p)
    (aref p 0))
  (defun (setf p-index) (n p)
    (setf (aref p 0) n))
  (defun p-current-array (p)
    (aref p 1))
  (defun (setf p-current-array) (n p)
    (setf (aref p 1) n))
  (defun p-data (p)
    (aref p 2))
  (defun (setf p-data) (n p)
    (setf (aref p 2) n))
  (declaim (notinline p-index (setf p-index)
		      p-current-array (setf p-current-array)
		      p-data (setf p-data))))

(defmacro backwards-array-iterator3 (index-var array-var completion-form)
  (let ((new-index (gensym))
	(new-array (gensym)))
    `(decf-unless-zerop
      ,index-var
      (multiple-value-bind (,new-index ,new-array) ,completion-form
	(declare (type fixnum ,new-index)
		 (type simple-vector ,new-array))
	(setf ,index-var ,new-index
	      ,array-var ,new-array)))))

(progn
  (declaim (inline foo3 foo2 foo1 foo0))
  (locally (declare (optimize (speed 3) (safety 0))
		    (inline p-index (setf p-index)
			    p-current-array (setf p-current-array)
			    p-data (setf p-data)
			    array-array-length array-array-array)
		    (notinline foo3 foo2 foo1 foo0))
    (progn
      (declaim (ftype (function (t)
				(values fixnum t))
		      foo3))
      (defun foo3 (x)
	(values -1 x)))
    
    (progn
      (declaim (ftype (function (simple-vector)
				(values fixnum simple-vector))
		      foo2)) 
      (defun foo2 (p)
	(with-load-unload ((meta-index (p-index p) fixnum)
			   (array-array (p-current-array p)))
	  
	  (backwards-array-iterator3 meta-index array-array (foo3 (p-data p)))
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
		    (values size data))))))))

    (progn
      (declaim (ftype (function (simple-vector)
				(values fixnum simple-vector))
		      foo1))
      (defun foo1 (p)
	(with-load-unload ((chunk-index (p-index p) fixnum)
			   (array (p-current-array p) (or null simple-vector)))
	  
	  (backwards-array-iterator3 chunk-index array (foo2 (p-data p)))	       
	  (values 
	   (1- +size+)
	   (let ((next-array (aref array chunk-index)))
	     (if next-array
		 next-array
		 (let ((newarray (create-scratch-array)))
		   (setf (aref array chunk-index) newarray)
		   newarray)))))))

    (progn
      (declaim (ftype (function (simple-vector t)
				(values))
		      foo0))	  
      (defun foo0 (p x)
	(with-load-unload ((sub-array (p-current-array p) (or null simple-vector))
			   (index (p-index p) fixnum))
	  
	  (backwards-array-iterator3 index sub-array (foo1 (p-data p)))	       
	  (setf (aref sub-array index) x)))))
  
  (declaim (notinline foo3 foo2 foo1 foo0)))

(defmacro with-flhat-writer ((array-name index-name next-func-name) p &body body)
  (let ((next-func (gensym))
	(writer (gensym)))
    `(let ((,writer ,p))
       (declare (type simple-vector ,writer)) 
       (with-load-unload ((,array-name (p-current-array ,writer) simple-vector)
			  (,index-name (p-index ,writer) fixnum))
	 (macrolet
	     ((,next-func-name ()
		'(backwards-array-iterator3 ,index-name ,array-name
		  (let ((,next-func (aref ,writer 3)))
		    (declare (type (function (t) (values fixnum simple-vector))
				   ,next-func))
		    (funcall ,next-func (p-data ,writer))))))	       
	   ,@body)))))

(defmacro with-array-iterator ((name value-place iterator) &body body)
  (let ((array-name (gensym))
	(index-name (gensym)))
    `(with-flhat-writer (,array-name ,index-name ,name) ,iterator
       (symbol-macrolet ((,value-place (aref ,array-name ,index-name)))
	 ,@body))))

(defmacro with-flhat-iterator ((name value-place flhat) &body body)
  (let ((iterator-name (gensym)))
    `(let ((,iterator-name (make-flhat-pointer ,flhat)))
       (with-array-iterator (,name ,value-place ,iterator-name)
			    ,@body))))

(defun make-flhat-pointer (flhat)
  (vector 0 nil
	  (vector 0 nil
		  (vector 0 nil flhat))
	  #'foo1))

(defun testwtf ()
  (declare (optimize (speed 3) (safety 0))
	   (inline foo0))
  (let ((bar (make-flhat-pointer *flhat*)))
    (dotimes (x (expt 10 7))
      (foo0 bar x))))

(defparameter *flhat* (make-array-array))

(defun testwtf2 () 
  (declare (optimize (speed 3) (safety 0))
	   (inline p-index (setf p-index)
		   p-current-array (setf p-current-array)
		   p-data (setf p-data)
		   array-array-length array-array-array))
  (with-flhat-iterator (next place *flhat*)
    (dotimes (x (expt 10 7))
      (next)
      (setf place x))
    (values)))
