(defun testwtf  ()
  (declare (optimize (speed 3) (safety 0))
	   (inline vertex-attrib))
  (zero-scratch-fill)
  (if t
      (dotimes (x (expt 10 6))
	(*vertex-attrib* (mod x 16) x))
      (let ((value *attrib-scratch*)
	    (value2 *attrib-scratch-fill*))
	(dotimes (x (expt 10 6))
	  (vertex-attrib value value2 (mod x 16) x)))))


;;;[ function call + data ] vs funcallable closure
;;;conclusion: stay away from closures if fast code, inlining with no function
;;;overhead, no float boxing on x86 on sbcl, is what is wanted. use vectors?
;;;instead? but what about structs? vectors = fast and flexible, but lambdas
;;;are also flexible? but flexible often means weak - flexibility comes at the
;;;price of strength and vice versa?

(defun gen-fillers (table result)
  (map-into result (lambda (x) (flhat:make-setter x 0 0)) table))
(defparameter *attrib-scratch-fillers* (make-array 16 :element-type t))
(defun reset-scratch-filler ()
  (gen-fillers *attrib-scratch* *attrib-scratch-fillers*))

(defun testwtf2 ()
  (declare (optimize (speed 3) (safety 0))
	   (inline vertex-attrib2))
  (reset-scratch-filler)
  (if t      
       (dotimes (x (expt 10 6))
	 (*vertex-attrib2* (mod x 16) x))
       (let ((value2 *attrib-scratch-fillers*))
	 (dotimes (x (expt 10 6))
	   (vertex-attrib2 value2 (mod x 16) x)))))
(progn
  (declaim (inline vertex-attrib2))
  (locally (declare (optimize (speed 3) (safety 0)))
    (defun vertex-attrib2 (filler-funcs attrib-location value)
      (declare (type simple-vector filler-funcs)
	       (type fixnum attrib-location))
      (let ((func (aref filler-funcs attrib-location)))
	(declare (type (function (t)) func))
	(funcall func value))))
  (declaim (notinline vertex-attrib2)))
(defmacro *vertex-attrib2* (attrib-location value)
  (vertex-attrib2 *attrib-scratch-fillers* attrib-location value))

(progno
 (defun make-attrib-table (array)
   (map-into array #'flhat:make-array-array))

 (defparameter *attrib-scratch* (make-array 16))
 (defparameter *attrib-scratch-fill* (make-array 16 :element-type '(unsigned-byte 32) :initial-element 0))

 (defun zero-scratch-fill ()
   (fill *attrib-scratch-fill* 0))

 (defun destroy-attrib-table ()
   (make-attrib-table *attrib-scratch*)
   (reset-all-mesh-scratches))

 (defun reset-all-mesh-scratches ()
   (zero-scratch-fill)
   (values))

 (progn
   (declaim (inline vertex-attrib))
   (locally (declare (optimize (speed 3) (safety 0))
		     (inline flhat:sets))
     (defun vertex-attrib (attrib-table table-fill attrib-location value)
       (declare (type (simple-array (unsigned-byte 32) (16)) table-fill)
		(type simple-vector attrib-table))
       (let ((fill (aref table-fill attrib-location))
	     (single-attrib (aref attrib-table attrib-location)))
	 (declare (type (unsigned-byte 32) fill))
	 (flhat:sets single-attrib fill value)
	 (setf (aref table-fill attrib-location) (1+ fill)))))
   (declaim (notinline vertex-attrib)))

 (defmacro *vertex-attrib* (&body pairs)
   (let ((acc nil))
     (alexandria:with-gensyms (fill buffer)
       (dolist (pair pairs)
	 (destructuring-bind (attrib-location-form &rest values) pair
	   (alexandria:with-gensyms (loc)
	     (let ((pair-acc (list `((,loc ,attrib-location-form))'let)))
	       (dolist (value values)
		 (push `(vertex-attrib ,buffer ,fill ,loc ,value) pair-acc))
	       (push (nreverse pair-acc) acc)))))
       (list* 'let `((,buffer *attrib-scratch*)
		     (,fill *attrib-scratch-fill*))
	      (nreverse acc)))))


 (defun draw-background2 ()
   (let ((distance 0.99999997))
     (*vertex-attrib* (2 0f0 0f0
			 1f0 0f0
			 1f0 1f0
			 0f0 1f0)
		      (0 -1.0 -1.0 distance
			 1.0 -1.0 distance
			 1.0 1.0 distance
			 -1.0 1.0 distance)
		      (8 1f0
			 1f0
			 1f0
			 1f0)))))
 
