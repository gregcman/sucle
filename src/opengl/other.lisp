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

(deflazy:deflazy gl-context ()
  (unless glhelp::*gl-context*
    (error "no opengl context you idiot!")))

(export '(deflazy-gl))
(defmacro deflazy-gl (name (&rest deps) &rest gen-forms)
  "for objects that should be forgotten because they were
not made in the current OpenGL context, so they are garbage"
  `(deflazy:deflazy ,name (,@deps gl-context)
     (declare (ignorable gl-context))
     ,@gen-forms))

(defmethod deflazy::cleanup-node-value ((object glhelp::gl-object))
  (when (glhelp:alive-p object)
    (glhelp::gl-delete* object)))

(defmacro with-gl-context ((gl-proc-address) &body body)
  `(unwind-protect (progn
		     (setf %gl:*gl-get-proc-address* ,gl-proc-address)
		     (setf glhelp::*gl-context* (cons "gl-context" "token"))
		     (setf glhelp::*gl-version* (gl:get-string :version))
		     (setf glhelp::*gl-version-substring*
			   (subseq glhelp::*gl-version* 0 3))
		     (setf glslgen::*glsl-version* (glhelp::glsl-gl-version))
		     (deflazy:refresh 'gl-context t)
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

(export '(set-render-area))
(defun set-render-area (x y width height)
  (gl:viewport x y width height)
  (gl:scissor x y width height))
