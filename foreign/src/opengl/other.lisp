(in-package :glhelp)

(defmacro with-gl-list (&body body)
  (let ((list-sym (gensym)))
    `(let ((,list-sym (gl:gen-lists 1)))
       (unwind-protect
	    (progn (gl:new-list ,list-sym :compile)
		   ,@body)
	 (gl:end-list))
       ,list-sym)))

(export '(with-gl-list))

(defmacro with-gl-context (&body body)
  `(unwind-protect (progn
		     (setf glhelp::*gl-context* (cons "gl-context" "token"))
		     ,@body)
     (setf glhelp::*gl-context* nil)))

(export '(with-gl-context))

(defparameter *gl-primitives*
  (list
   :points
   :lines
   :line-strip
   :line-loop
   :triangles
   :triangle-strip
   :triangle-fan
   :quads
   :quad-strip
   :polygon))
