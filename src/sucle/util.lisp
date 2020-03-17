(in-package :sucle)
;;multiple-value-call synonym
(defmacro mvc (fun arg &rest args)
  `(multiple-value-call ,fun ,arg ,@args))
(declaim (inline spread))
(defun spread (&optional (obj (sb-cga:vec 1.0 2.0 3.0)))
  (declare (optimize (speed 3) (safety 0)))
  (symbol-macrolet
      ((array-code
	(let ((size (array-total-size obj)))
	  (case size
	    ;;The first four are faster.
	    ;;FIXME:optimize for list vs sequences
	    (0 (values))
	    (1 (values
		(aref obj 0)))
	    (2 (values
		(aref obj 0)
		(aref obj 1)))
	    (3 (values
		(aref obj 0)
		(aref obj 1)
		(aref obj 2)))
	    (4 (values
		(aref obj 0)
		(aref obj 1)
		(aref obj 2)
		(aref obj 3)))
	    ;;The slow option
	    (otherwise (values-list (coerce obj 'list)))))))
    (etypecase obj
      (list
       (let ((size (list-length obj)))
	 (if size
	     (values-list obj)
	     (error "Attempting to call values-list on a circular list!"))))
      (simple-string array-code)
      ((simple-array single-float (*)) array-code)
      ((simple-array t (*)) array-code))))
(defun test-spread ()
  (declare (optimize (speed 3)
		     (safety 0)))
  (time
   (dotimes (x (expt 10 6))
     (spread))))
;;;;
(defmacro floatf (&rest args)
  `(progn
     ,@(mapcar (lambda (arg)
		 `(setf ,arg (floatify ,arg)))
	       args)))
