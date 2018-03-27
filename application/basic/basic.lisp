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
    flat-shader
    flat-texture-shader-source
    flat-texture-shader
    cons-png
    cons-texture))

(defparameter *pen-color* (list 1.0 0.0 0.0 1.0))
(defun app ()
  (incf *ticks*)
  (map nil #'application::reload-if-dirty *reloadables*)
  (when (window::skey-j-p (window::keyval :q))
    (application::quit))
  (when (window::skey-j-p (window::keyval :r))
    (application::reload 'cons-texture))
  (when (window::skey-j-p (window::keyval :a))
    (music::reset-listener)
    (music::play-at (merge-pathnames "wilhelm_scream.wav" *this-directory*) 0.0 0.0 0.0 1.0 1.0))
  (application::%set-render-area 0 0 window:*width* window:*height*)
  (gl:clear-color 0.5 0.5 0.5 0.0)
  (gl:clear :color-buffer-bit)
  (gl:disable :cull-face)
  (gl:line-width 10.0)
  (gl:point-size 10.0)
  (let ((program (getfnc 'flat-shader)))
    (glhelp::use-gl-program program))
  (gl:with-primitive :line-loop
    (gl:color 1.0 0.0 0.0)
    (gl:vertex -1.0 -1.0)
    (gl:color 0.0 1.0 0.0)
    (gl:vertex 0.0 1.0)
    (gl:color 0.0 0.0 1.0)
    (gl:vertex 1.0 -1.0))
  (let ((foo (sin (/ *ticks* 60.0))))
    (gl:with-primitive :line-loop
      (draw-quad -0.5 (+ foo -0.5)
		 0.5 (+ foo 0.5)))
    (let ((*pen-color* '(0.0 0.0 0.0 1.0)))
      (gl:line-width 2.0)
      (gl:with-primitive :triangle-fan
	(draw-circle 0.0 0.0 (* foo 1.0) 32 0.0)))
    (let ((program (getfnc 'flat-texture-shader)))
      (glhelp::use-gl-program program)
      (glhelp:with-uniforms uniform program
	(gl:disable :blend)
	(gl:uniformi (uniform 'sampler) 0)
	(glhelp::set-active-texture 0)
	(gl:bind-texture :texture-2d
			 (glhelp::handle (getfnc 'cons-texture))))
      (let ((*pen-color* '(1.0 0.0 1.0 1.0)))
	(gl:with-primitive :quads
	  (draw-textured-quad 0.5 0.5 0.9 0.9 0.0 0.0 1.0 1.0))))))

(defun draw-textured-quad (x0 y0 x1 y1 s0 t0 s1 t1)
  (destructuring-bind (r g b a) *pen-color*
    (gl:color r g b a)
    (gl:tex-coord s0 t0)
    (gl:vertex x0 y0)

    (gl:color r g b a)
    (gl:tex-coord s0 t1)
    (gl:vertex x0 y1)   

    (gl:color r g b a)
    (gl:tex-coord s1 t1)
    (gl:vertex x1 y1)

    (gl:color r g b a)
    (gl:tex-coord s1 t0)
    (gl:vertex x1 y0)
    ))

(defun draw-quad (x0 y0 x1 y1)
  (destructuring-bind (r g b a) *pen-color*
    (gl:color r g b a)
    (gl:vertex x0 y0)
    (gl:color r g b a)
    (gl:vertex x0 y1)
    (gl:color r g b a)
    (gl:vertex x1 y1)
    (gl:color r g b a)
    (gl:vertex x1 y0)))

(defmacro floatify (x)
  `(coerce ,x 'single-float))

(defmacro complex-cis ((place placei theta) &body body)
  `(let ((,place (cos ,theta))
	 (,placei (sin ,theta)))
     ,@body))
(defmacro complex-multiply (a b c d place placei)
  `(psetf ,place (- (* ,a ,c)
		    (* ,b ,d))
	  ,placei (+ (* ,a ,d)
		     (* ,c ,b))))
(defmacro complex-add ((a b c d place placei) &body body)
  `(let ((,place (+ ,a ,c))
	 (,placei (+ ,b ,d)))
     ,@body))
(defun draw-circle (x y radius divisions start-angle)
  (destructuring-bind (r g b a) *pen-color*
    (complex-cis (offset offseti start-angle)
      (complex-multiply offset offseti radius 0.0 offset offseti)
      
      (complex-cis (d di (/ (* 2.0 (load-time-value
				    (floatify pi)))
			    divisions))
	(dotimes (i (floor divisions))
	  (complex-add (offset offseti x y point-x point-y)
	    (gl:color r g b a)
	    (gl:vertex point-x point-y))
	  (complex-multiply offset offseti d di offset offseti))))))

(progn
  (deflazy flat-shader-source ()
    (glslgen:ashader
     :version 120
     :vs
     (glslgen2::make-shader-stage
      :out '((value-out "vec4"))
      :in '((position "vec4")
	    (value "vec4"))
      :program
      '(defun "main" void ()
	(= "gl_Position" position)
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
     '()))
  (deflazy flat-shader (flat-shader-source)
    (glhelp::create-gl-program flat-shader-source)))

(progn
  (deflazy flat-texture-shader-source ()
    (glslgen:ashader
     :version 120
     :vs
     (glslgen2::make-shader-stage
      :out '((value-out "vec4")
	     (tex-out "vec2"))
      :in '((position "vec4")
	    (tex "vec2")
	    (value "vec4"))
      :program
      '(defun "main" void ()
	(= "gl_Position" position)
	(= value-out value)
	(= tex-out tex)))
     :frag
     (glslgen2::make-shader-stage
      :in '((value "vec4")
	    (tex "vec2")
	    (sampler "sampler2D"))
      :program
      '(defun "main" void ()
	(/**/ vec4 pixdata)
	(= pixdata ("texture2D" sampler tex))
	(=
	 :gl-frag-color
	 (* pixdata value)
	 )))
     :attributes
     '((position . 0)
       (tex . 8)
       (value . 3))
     :varyings
     '((value-out . value)
       (tex-out . tex))
     :uniforms
     '((sampler (:fragment-shader sampler)))))
  (deflazy flat-texture-shader (flat-texture-shader-source)
    (glhelp::create-gl-program flat-texture-shader-source)))

(progn
  (deflazy cons-png ()
    (flip-image:flip-image
     (opticl:read-png-file
      (filesystem-util:rebase-path #P"cons-cell.png" *this-directory*))))
  (deflazy cons-texture (cons-png)
    (make-instance
     'glhelp::gl-texture
     :handle
     (prog1
	 (glhelp:pic-texture
	  cons-png
	  :rgb)
       (glhelp:apply-tex-params
	(quote ((:texture-min-filter . :nearest)
		(:texture-mag-filter . :nearest)
		(:texture-wrap-s . :clamp)
		(:texture-wrap-t . :clamp))))))))
