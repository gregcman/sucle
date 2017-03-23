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

 
