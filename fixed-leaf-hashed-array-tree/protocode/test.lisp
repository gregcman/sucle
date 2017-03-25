(in-package :fixed-leaf-hashed-array-tree)

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
(progn
  (declaim (inline p-index (setf p-index)
		   p-current-array (setf p-current-array)
		   p-data (setf p-data)
		   p-func (setf p-func)))
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
  (defun p-func (p)
    (aref p 3))
  (defun (setf p-func) (n p)
    (setf (aref p 3) n)))

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
  `(with-let-mapped-places ((,array (p-current-array ,iterator) ,array-type)
			    (,index (p-index ,iterator) ,index-type))
     (flet ((,next ()
	      (backwards-array-iterator3
	       ,index ,array
	       (iterator-transfer ,iterator (values ,index-type ,array-type)))))
       (symbol-macrolet ((,place (aref ,array ,index)))
	 ,@body))))

(declaim (ftype (function (simple-vector))
		next-index)
	 (ftype (function (simple-vector)
			  (values fixnum simple-vector))
		next-subarray)
	 (ftype (function (simple-vector)
			  (values fixnum simple-vector))
		next-flhat))

(with-unsafe-speed
  (defun next-index (p)
    (with-bound-iterator (next place (array) (index)) p
      (next))))




(with-unsafe-speed
  (defun next-init (p)
    (values -1 p)))

(with-unsafe-speed
  (defun next-flhat (p)
    (with-bound-iterator (next place (flhat cons) (meta-index)) p
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
  (declaim (ftype (function (cons) (simple-vector 4)) make-flhat-pointer))
  (defun make-flhat-pointer (flhat)
    (let ((data (vector 0 nil
			(vector 0 nil
				(vector 0 nil flhat (function next-init))
				(function next-flhat))
			(function next-subarray))))
      data)))


(defmacro with-flhat-iterator ((name value-place flhat) &body body)
  (let ((iterator (gensym))
	(array (gensym))
	(index (gensym)))
    `(let ((,iterator (make-flhat-pointer ,flhat)))
       (with-bound-iterator (,name ,value-place (,array) (,index)) ,iterator
	 ,@body))))

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
