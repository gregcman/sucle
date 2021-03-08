(in-package :sucle)
;;multiple-value-call synonym
(defun empty-air-p (thing)
  (or (eql 0 thing)
      (null thing)))
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

;;;;
(defun run-button (pair)
  ;;((:key :pressed #\Space) . function)
  (when (apply 'window:button (car pair))
    (funcall (cdr pair))))
(defun run-buttons (pairs)
  (mapc 'run-button pairs))

;;;;[FIXME]The point of this is to reduce the amount of bits to store the hitbox.
;;;;Why? because when there is an inexact number, like 0.3, there are bits at the end which
;;;;get chopped off or something, thus leading to strange clipping.
;;;;This effectively reduces the precision, giving leeway for math operations.
;;;;My prediction could be wrong though.
(defun round-to-nearest (x &optional (n (load-time-value (/ 1.0 128.0))))
  (* n (round (/ x n))))

;;;;************************************************************************;;;;
;;;;<BOXES?>
(defun create-aabb (&optional (maxx 1.0) (maxy maxx) (maxz maxx)
			       (minx (- maxx)) (miny (- maxy)) (minz (- maxz)))
	 (floatf maxx maxy maxz minx miny minz)
	 (aabbcc:make-aabb
	  :minx (round-to-nearest minx)
	  :maxx (round-to-nearest maxx)
	  :miny (round-to-nearest miny)
	  :maxy (round-to-nearest maxy)
	  :minz (round-to-nearest minz)
	  :maxz (round-to-nearest maxz)))
