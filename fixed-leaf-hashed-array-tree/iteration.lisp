(in-package :fixed-leaf-hashed-array-tree)

(declaim (ftype (function (simple-vector))
		next-index)
	 (ftype (function (simple-vector)
			  (values fixnum simple-vector))
		next-subarray)
	 (ftype (function (simple-vector)
			  (values fixnum simple-vector))
		next-flhat))

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
  (declaim (ftype (function (cons) (simple-vector 4)) make-flhat-iterator))
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
	    (p-index value-iter) offset-index)
      (let ((array (flhat-data flhat)))
	(setf (p-array value-iter)
	      (let ((sub-array (aref array chunk-index)))
		(if sub-array
		    sub-array
		    (let ((new-array (create-scratch-array)))
		      (setf (aref array chunk-index)
			    new-array)))))))))
