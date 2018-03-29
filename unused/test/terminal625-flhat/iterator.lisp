(in-package :reverse-array-array)

(declaim (ftype (function (iterator))
		next-index)
	 (ftype (function (iterator)
			  (values fixnum simple-vector))
		next-subarray)
	 (ftype (function (iterator)
			  (values fixnum simple-vector))
		next-raa))

(with-unsafe-speed
  (defun next-init (p)
    (values -1 p)))

(with-unsafe-speed
  (defun next-raa (p)
    (with-bound-iterator (next place (raa raa) (meta-index)) p
      (next)
      
      (if (= -1 meta-index)
	  (let ((size (1- (raa-length raa))))
	    (declare (type fixnum size))
	    (values size (raa-data raa)))
	  (let ((old-size (raa-length raa)))
	    (declare (type fixnum old-size))
	    (multiple-value-bind (size data) (reverse-fit-resize raa old-size)
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
  (declaim (ftype (function (raa) iterator) make-raa-iterator))
  (defun make-raa-iterator (raa)
    (let* ((raa-iter (make-zeroed-iterator #'next-init raa))
	   (array-iter (make-zeroed-iterator #'next-raa raa-iter))
	   (value-iter (make-zeroed-iterator #'next-subarray array-iter)))
      value-iter)))


(defmacro with-raa-iterator ((next-fun value-place raa) &body body)
  (let ((iterator (gensym))
	(array (gensym))
	(index (gensym)))
    `(let ((,iterator (make-raa-iterator ,raa)))
       (with-bound-iterator (,next-fun ,value-place (,array) (,index)) ,iterator
	 ,@body))))

(defun relocate-iterator (value-iter n)
  (let* ((array-iter (p-data value-iter))
	 (raa-iter (p-data array-iter))
	 (raa (p-data raa-iter)))
    (multiple-value-bind (chunk-index offset-index) (xindex raa n)
      (setf (p-index array-iter) chunk-index
	    (p-index value-iter) offset-index
	    (p-array raa-iter) raa
	    (p-index raa-iter) -1)
      (let ((array (raa-data raa)))
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
	 (raa-iter (p-data array-iter)))
    (setf (p-index value-iter) 0
	  (p-array value-iter) nil
	  (p-index array-iter) 0
	  (p-array array-iter) nil
	  (p-index raa-iter) 0
	  (p-array raa-iter) nil)
    value-iter))

(defun iterator-position (value-iter)
  (let* ((array-iter (p-data value-iter))
	 (raa-iter (p-data array-iter))
	 (raa (p-data raa-iter)))
    (let ((value-index (p-index value-iter))
	  (array-index (p-index array-iter))
	  (total-size (raa-length raa)))
      (+ (ash (- total-size array-index 1) +log-size+)
	 (- +size+ value-index 1))))) 
