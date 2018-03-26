(defpackage #:basic
  (:use #:cl #:utility #:application))
(in-package :basic)

(defun per-frame (&optional session)
  (declare (ignorable session))
  (app))
(defun start ()
  (let ((application::*argument-values*
	 (list nil
	       *window-start-width*
	       *window-start-height*
	       *window-start-title*)))
    (setf application::*trampoline*
	  '(per-frame text-sub::per-frame
	    ))
    (application::main)))
(defvar *this-directory* (filesystem-util:this-directory))

(defparameter *window-start-height* 480)
(defparameter *window-start-width* 720)
(defparameter *window-start-title* "basic app")
(defparameter *ticks* 0)

(defparameter *reloadables*
  '(flat-shader-source
    flat-shader))

(defun app ()
  (incf *ticks*)
  (map nil #'application::reload-if-dirty *reloadables*)
  (when (window::skey-j-p (window::keyval :q))
    (application::quit))
  (when (window::skey-j-p (window::keyval :a))
    (music::reset-listener)
    (music::play-at (merge-pathnames "wilhelm_scream.wav" *this-directory*) 0.0 0.0 0.0 1.0 1.0))
  (application::%set-render-area 0 0 window:*width* window:*height*)
  (gl:clear-color 1.0 1.0 1.0 0.0)
  (gl:clear :color-buffer-bit)
  (gl:line-width 10.0)
  (let ((program (getfnc 'flat-shader)))
    (glhelp::use-gl-program program))
  (gl:with-primitive :triangles ;line-loop
    (gl:color 1.0 0.0 0.0)
    (gl:vertex -1.0 -1.0)
    (gl:color 0.0 1.0 0.0)
    (gl:vertex 0.0 1.0)
    (gl:color 0.0 0.0 1.0)
    (gl:vertex 1.0 -1.0)))

(deflazy flat-shader-source ()
  (glslgen:ashader
   :version 120
   :vs
   (glslgen2::make-shader-stage
    :out '((value-out "vec4"))
    :in '((position "vec4")
	  (value "vec4")
	  ;(projection-model-view "mat4")
	  )
    :program
    '(defun "main" void ()
      (= "gl_Position" position ;(* projection-model-view position)
       )
      (= value-out value)))
   :frag
   (glslgen2::make-shader-stage
    :in '((value "vec4"))
    :program
    '(defun "main" void ()	 
      (=
       :gl-frag-color
       value
       )))
   :attributes
   '((position . 0) 
     (value . 3))
   :varyings
   '((value-out . value))
   :uniforms
   '(;(:pmv (:vertex-shader projection-model-view))
     )))

(deflazy flat-shader (flat-shader-source)
  (glhelp::create-gl-program flat-shader-source))

