(in-package :fixed-leaf-hashed-array-tree)

(progn
  (declaim (ftype (function (cons fixnum fixnum)) %make-setter))
  (locally (declare (optimize (speed 3) (safety 0)))
    (defun %make-setter (array-array index chunk-index)
      (declare (type fixnum index chunk-index))
      (let ((array (array-array-array array-array))
	    (array-length (array-array-length array-array)))
	(let ((sub-array (aref array chunk-index)))
	  (declare (type (or null simple-vector) sub-array))
	  (unless sub-array
	    (setf sub-array (create-scratch-array))
	    (setf (aref array chunk-index) sub-array))
	  (labels ((func (x)
		     (setf (aref sub-array index) x)
		     (incf index)
		     (when (= index +size+)
		       (setf index 0)
		       (progn
			 (let ((next (1+ chunk-index)))
			   (declare (type fixnum next))
			   (setf chunk-index next)
			   (when (= next array-length)
			     (setf (values array-length array)
				   (fit-resize array-array next))))
			 (let ((next-array (aref array chunk-index)))
			   (declare (type (or null simple-vector) next-array))
			   (if next-array
			       (setf sub-array next-array)
			       (let ((newarray (create-scratch-array)))
				 (setf sub-array newarray)
				 (setf (aref array chunk-index) newarray))))))))
	    (dotimes (x (expt 10 6))
	      (func x))))))))


(defparameter *scratch* (make-array-array 1))

(defun testwtf ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((counter (make-setter *scratch* 0 0))) 
    (dotimes (x (expt 10 6))
      (funcall counter x))))

(defun testwtf? ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((counter (constantly nil))) 
    (dotimes (x (expt 10 6))
      (funcall counter 0))))

(defun testwtf2 ()
  (declare (optimize (speed 3) (safety 0)))
  (%make-setter *scratch* 0 0) )

(progn
  (declaim (ftype (function (cons fixnum fixnum)
			    (function (t))) make-setter))
  (locally (declare (optimize (speed 3) (safety 0)))
    (defun make-setter3 (array-array index chunk-index)
      (declare (type fixnum index chunk-index))
      (let ((array (array-array-array array-array))
	    (array-length (array-array-length array-array)))
	(let ((sub-array (aref array chunk-index)))
	  (declare (type (or null simple-vector) sub-array))
	  (unless sub-array
	    (setf sub-array (create-scratch-array))
	    (setf (aref array chunk-index) sub-array))
	  (labels ((end-of-chunk ()
		     (setf index +size+)
		     (progn
		       (let ((next (1+ chunk-index)))
			 (declare (type fixnum next))
			 (setf chunk-index next)
			 (when (= next array-length)
			   (setf (values array-length array)
				 (fit-resize array-array next))))
		       (let ((next-array (aref array chunk-index)))
			 (declare (type (or null simple-vector) next-array))
			 (if next-array
			     (setf sub-array next-array)
			     (let ((newarray (create-scratch-array)))
			       (setf sub-array newarray)
			       (setf (aref array chunk-index) newarray))))))
		   (func (x)
		     (decf index)
		     (setf (aref sub-array index) x)
		     (when (zerop index)
		       (end-of-chunk))))
	    #'func))))))

(defun testwtf3 ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((counter (make-setter3 *scratch* 0 0))) 
    (dotimes (x (expt 10 6))
      (funcall counter x))))

(defun wot (n)
  (declare (type fixnum n)
	   (optimize (speed 3) (safety 0)))
  (if t
      (= 1024 n)
      (zerop n))) 

(progn  (progn
	    (declaim (ftype (function (simple-vector fixnum t))
			    %sets))
	    (declaim (inline %sets))
	    (locally (declare (optimize (speed 3) (safety 0))
			      (inline offset-index chunk-index get-array-or-nil))
	      (defun %sets (array n value)
		(let ((hashcode (chunk-index n)))
		  (let ((sub-array (get-array-or-nil array hashcode)))
		    (if sub-array
			(setf (aref sub-array (offset-index n)) value)
			(let ((new-array (create-scratch-array)))
			  (setf (aref new-array (offset-index n)) value)
			  (setf (aref array hashcode) new-array)))))))
	    (declaim (notinline %sets)))

	  (progn
	    (declaim (ftype (function (simple-vector fixnum) (or t null)) %gets))
	    (locally (declare (optimize (speed 3) (safety 0))
			      (inline offset-index chunk-index get-array-or-nil))
	      
	      (defun %gets (array n)
		(let ((hashcode (chunk-index n)))
		  (let ((sub-array (get-array-or-nil array hashcode)))
		    (if array
			(let ((chunk-index (offset-index n)))
			  (aref sub-array chunk-index)))))))))

(defmacro backwards-array-iterator (index-var array-var completion-form)
  `(if (zerop ,index-var)
       (setf (values ,index-var ,array-var)
	     ,completion-form)
       (decf ,index-var)))

(defun %make-iterator (array-array)
  (declare (optimize (speed 3) (safety 0))
	   (inline array-array-array array-array-length))
  (let ((array nil)
	(sub-array nil)
	(meta-index 0)
	(chunk-index 0)
	(index 0))
    (declare (type (or null cons) array-array)
	     (type (or null simple-vector) array sub-array)
	     (type fixnum meta-index chunk-index index))
    (labels ((func (x)
	       (backwards-array-iterator index sub-array (next-sub-array))
	       
	       (setf (aref sub-array index) x))
	     
	     (next-sub-array ()
	       (backwards-array-iterator chunk-index array (next-array-array))
		 
	       (values 
		(1- +size+)
		(let ((next-array (aref array chunk-index)))
		  (declare (type (or null simple-vector) next-array))
		  (if next-array
		      next-array
		      (let ((newarray (create-scratch-array)))
			(setf (aref array chunk-index) newarray)
			newarray)))))
	     
	     (next-array-array ()
	       (backwards-array-iterator meta-index array-array (init))

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
			 (values size data))))))
	     
	     (init ()
	       (values -1 array-array)))
      (declare (ftype (function (t) (values)) func)
	       (ftype (function () (values fixnum simple-vector)) next-sub-array)
	       (ftype (function () (values fixnum simple-vector)) next-array-array)
	       (ftype (function () (values fixnum cons)) init))
      (if nil
	  #'func
	  (dotimes (x (expt 10 6))
	    (func x))))))

(defmacro backwards-array-iterator (index-var array-var completion-form)
  `(if (zerop ,index-var)
       (setf (values ,index-var ,array-var)
	     ,completion-form)
       (decf ,index-var)))

(defun %make-iterator2 (flhat)
  
  (declare (optimize (speed 3) (safety 0))
	   (inline array-array-array array-array-length))
    
  (labels ((init ()
	     (values -1 flhat)))
    (declare (ftype (function () (values fixnum cons)) init))

    (let ((array nil)
	  (meta-index 0)
	  (array-array nil))
      (declare (type (or null simple-vector) array)
	       (type fixnum meta-index)
	       (type (or null cons) array-array)) 
    (labels ((next-array-array ()
	       (backwards-array-iterator meta-index array-array (init))

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
			 (values size data)))))))
      (declare (ftype (function () (values fixnum simple-vector)) next-array-array))

      (let ((chunk-index 0))
	(declare (type fixnum chunk-index))
	(labels ((next-sub-array ()
		   (backwards-array-iterator chunk-index array (next-array-array))	       
		   (values 
		    (1- +size+)
		    (let ((next-array (aref array chunk-index)))
		      (declare (type (or null simple-vector) next-array))
		      (if next-array
			  next-array
			  (let ((newarray (create-scratch-array)))
			    (setf (aref array chunk-index) newarray)
			    newarray))))))
	  (declare (ftype (function () (values fixnum simple-vector)) next-sub-array))
	  
	  (let ((sub-array nil)
		(index 0))
	    (declare (type (or null simple-vector) sub-array)
		     (type fixnum index))
	    (labels ((func (x)
		       (backwards-array-iterator index sub-array (next-sub-array))	       
		       (setf (aref sub-array index) x)))
	      (declare (ftype (function (t) (values)) func))	  
	      (if nil
		  #'func
		  (dotimes (x (expt 10 6))
		    (func x)))))))))))




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

(defmacro backwards-array-iterator2 (index-value index-var array-value array-var completion-form)
  `(if (zerop ,index-value)
       (multiple-value-bind (new-index new-array) ,completion-form
	 (setf ,index-var new-index
	       ,index-value new-index
	       ,array-var new-array
	       ,array-value new-array))
       (let ((new-index (1- ,index-value)))
	 (declare (type fixnum new-index))
	 (setf ,index-var new-index
	       ,index-value new-index))))

(progn
  (declaim (inline foo3 foo2 foo1 foo0))
  (locally (declare (inline p-index (setf p-index)
			    p-current-array (setf p-current-array)
			    p-data (setf p-data)
			    array-array-array array-array-length)
		    (optimize (speed 3) (safety 0)))
    (progn
      (declaim (ftype (function (t) (values fixnum cons)) foo3))
      (defun foo3 (x)
	(values -1 x)))
    
    (progn
      (declaim (ftype (function (simple-vector) (values fixnum simple-vector)) foo2)) 
      (defun foo2 (p)
	(let ((meta-index (p-index p))
	      (array-array (p-current-array p)))
	  (declare (type fixnum meta-index)
		   (type (or null cons) array-array))
	  (backwards-array-iterator2 meta-index (p-index p)
				     array-array (p-current-array p) (foo3 (p-data p)))
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
      (declaim (ftype (function (simple-vector) (values fixnum simple-vector)) foo1))
      (defun foo1 (p)
	(let ((chunk-index (p-index p))
	      (array (p-current-array p)))
	  (declare (type fixnum chunk-index)
		   (type (or null simple-vector) array))
	  (backwards-array-iterator2 chunk-index (p-index p)
				     array (p-current-array p) (foo2 (p-data p)))	       
	  (values 
	   (1- +size+)
	   (let ((next-array (aref array chunk-index)))
	     (declare (type (or null simple-vector) next-array))
	     (if next-array
		 next-array
		 (let ((newarray (create-scratch-array)))
		   (setf (aref array chunk-index) newarray)
		   newarray)))))))

    (progn
      (declaim (ftype (function (simple-vector t) (values)) foo0))	  
      (defun foo0 (p x)
	(let ((sub-array (p-current-array p))
	      (index (p-index p)))
	  (declare (type (or null simple-vector) sub-array)
		   (type fixnum index))
	  (backwards-array-iterator2 index (p-index p)
				     sub-array (p-current-array p) (foo1 (p-data p)))	       
	  (setf (aref sub-array index) x)))))
  (declaim (notinline foo3 foo2 foo1 foo0)))

(defmacro backwards-array-iterator3 (index-var array-var completion-form)
  (let ((new-index (gensym))
	(new-array (gensym)))
    `(if (zerop ,index-var)
	 (multiple-value-bind (,new-index ,new-array) ,completion-form
	   (declare (type fixnum ,new-index)
		    (type simple-vector ,new-array))
	   (setf ,index-var ,new-index
		 ,array-var ,new-array))
	 (let ((,new-index (1- ,index-var)))
	   (declare (type fixnum ,new-index))
	   (setf ,index-var ,new-index)))))

(let ((next-func (aref ,writer 3)))
  (declare (type (function (t) (values fixnum simple-vector))
		 next-func))
  (funcall next-func (p-data ,writer)))

(defmacro with-flhat-writer ((array-name index-name next-func-name) p &body body)
  (let ((next-func (gensym))
	(writer (gensym)))
    `(let ((,writer ,p))
       (declare (type simple-vector ,writer)
		(inline p-current-array p-index p-data))
       (let ((,array-name (p-current-array ,writer))
	     (,index-name (p-index ,writer)))
	 (declare (type simple-vector ,array-name)
		  (type fixnum ,index-name)
		  (inline (setf p-current-array) (setf p-index)))
	 (macrolet
	     ((,next-func-name ()
		'(backwards-array-iterator3 ,index-name ,array-name
		  (let ((,next-func (aref ,writer 3)))
		    (declare (type (function (t) (values fixnum simple-vector))
				   ,next-func))
		    (funcall ,next-func (p-data ,writer))))))	       
	   ,@body)
	 (setf (p-current-array ,writer) ,array-name
	       (p-index ,writer) ,index-name)))))

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
  (let ((bar (make-flhat-pointer)))
    (dotimes (x (expt 10 6))
      (foo0 bar x))))

(defparameter *flhat* (make-array-array))

(defun testwtf2 () 
  (declare (optimize (speed 3) (safety 0)))
  (with-flhat-iterator (next place *flhat*)
    (dotimes (x (expt 10 6))
      (next)
      (setf place 0))
  (values)))
