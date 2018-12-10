(defpackage #:text-sub
  (:use #:cl
	#:application
	#:utility
	#:reverse-array-iterator-user))
(in-package #:text-sub)

(deflazy text-data (gl-context)
  (glhelp::make-gl-framebuffer 256 256))

(deflazy text-shader-source ()
  (glslgen:ashader
   :vs
   (glslgen2::make-shader-stage
    :out '((texcoord-out "vec2"))
    :in '((position "vec4")
	  (texcoord "vec2")
	  (projection-model-view "mat4"))
    :program
    '(defun "main" void ()
      (= "gl_Position" (* projection-model-view position))
      (= texcoord-out texcoord)))
   :frag
   (glslgen2::make-shader-stage
    :in '((texcoord "vec2")
	  (indirection "sampler2D")
	  (text-data "sampler2D")
	  (font-atlas ("vec4" 256))
	  (color-atlas ("vec4" 256))
	  (attributeatlas ("vec4" 256))
	  (font-texture "sampler2D"))
    :program
    '(defun "main" void ()

	 ;;;indirection
      (/**/ vec4 ind)
      (= ind ("texture2D" indirection texcoord))

      (/**/ vec4 raw)
      (= raw ("texture2D" text-data
	      (|.| ind "ba")))

      ;;where text changes go
      (/**/ ivec4 chardata)
      (= chardata
       (ivec4 (* 255.0 raw)))

      ;;font atlass coordinates
      (/**/ vec4 fontdata)
      (= fontdata
       ([]
	font-atlas
	(|.| chardata "r")))

      (/**/ vec4 attributedata)
      (= attributedata
       ([]
	attributeatlas
	(|.| chardata "a")))

      (/**/ vec2 texindexraw)
      (= texindexraw
       (mix (|.| fontdata "xy")
	(|.| fontdata "zw")
	(|.| ind "rg")
	))
      
      (/**/ vec2 texindex)
      (= texindex
       (vec2
	(* 0.5 (|.| texindexraw "x"))
	(|.| texindexraw "y")))
      
      ;;font lookup
      (/**/ vec4 pixcolor)
      (= pixcolor
       ("texture2D"
	font-texture
	texindex))
      
      (/**/ vec4 fin)
      (= fin
       (mix
	([] color-atlas (|.| chardata "g"))
	([] color-atlas (|.| chardata "b"))
	pixcolor))
      (= (|.| :gl-frag-color "rgb")
       (|.| fin "rgb"))
      (= (|.| :gl-frag-color "a")
       (*
	(|.| fin "a")
	(|.| raw "a")))
      ))
   :attributes
   '((position . 0) 
     (texcoord . 2))
   :varyings
   '((texcoord-out . texcoord))
   :uniforms
   '((:pmv (:vertex-shader projection-model-view))
     (indirection (:fragment-shader indirection))
     (attributedata (:fragment-shader attributeatlas))
     (text-data (:fragment-shader text-data))
     (color-data (:fragment-shader color-atlas))
     (font-data (:fragment-shader font-atlas))
     (font-texture (:fragment-shader font-texture)))))

(defvar *this-directory* (asdf:system-source-directory :text-subsystem))
(deflazy font-png ()
  (let ((array
	 (image-utility:read-png-file
	  (utility:rebase-path #P"font.png"
			       *this-directory*))))
    (destructuring-bind (w h) (array-dimensions array)
      (let ((new
	     (make-array (list w h 4) :element-type '(unsigned-byte 8) :initial-element 255)))
	(dobox ((width 0 w)
		(height 0 h))
	       (let ((value (aref array width height)))
		 (dotimes (i 3)
		   (setf (aref new width height i) value))))
	new))))
(deflazy font-texture (font-png gl-context)
  (prog1
      (make-instance
       'glhelp::gl-texture
       :handle
       (glhelp:pic-texture
	font-png
	:rgba
	))
    (glhelp:apply-tex-params
     (quote ((:texture-min-filter . :nearest
				  )
	     (:texture-mag-filter . :nearest
				  )
	     (:texture-wrap-s . :repeat)
	     (:texture-wrap-t . :repeat))))))

(defparameter *trans* (nsb-cga:scale* (/ 1.0 128.0) (/ 1.0 128.0) 1.0))
(defun retrans (x y &optional (trans *trans*))
  (setf (aref trans 12) (/ x 128.0)
	(aref trans 13) (/ y 128.0))
  trans)

(defmacro with-data-shader ((uniform-fun rebase-fun) &body body)
  (with-gensyms (program)
    `(let ((,program (getfnc 'flat-shader)))
       (glhelp::use-gl-program ,program)
       (gl:bind-framebuffer :framebuffer (glhelp::handle (getfnc 'text-data)))
       (glhelp:set-render-area 0 0 256 256)
       (glhelp:with-uniforms ,uniform-fun ,program
	 (flet ((,rebase-fun (x y)
		  (gl:uniform-matrix-4fv
		   (,uniform-fun :pmv)
		   (retrans x y)
		   nil)))
	   ,@body)))))

(defmacro with-text-shader ((uniform-fun) &body body)
  (with-gensyms (program)
    `(progn
         (getfnc 'render-normal-text-indirection)
	 (getfnc 'color-lookup)
	 (let ((,program (getfnc 'text-shader)))
	   (glhelp::use-gl-program ,program)
	   (glhelp:with-uniforms ,uniform-fun ,program
	     (progn
	       (gl:uniformi (,uniform-fun 'indirection) 0)
	       (glhelp::set-active-texture 0)
	       (gl:bind-texture :texture-2d
				(glhelp::texture (getfnc 'indirection))))
	     (progn
	       (gl:uniformi (,uniform-fun 'font-texture) 2)
	       (glhelp::set-active-texture 2)
	       (gl:bind-texture :texture-2d
				(glhelp::handle (getfnc 'font-texture))))
	     (progn
	       (gl:uniformi (,uniform-fun 'text-data) 1)
	       (glhelp::set-active-texture 1)
	       (gl:bind-texture :texture-2d
				(glhelp::texture (getfnc 'text-data))))
	     ,@body)))))

(deflazy fullscreen-quad (gl-context)
  (let ((a (scratch-buffer:my-iterator))
	(b (scratch-buffer:my-iterator))
	(len 0))
    (bind-iterator-out
     (pos single-float) a
     (bind-iterator-out
      (tex single-float) b
      (etouq (cons 'pos (axis-aligned-quads:quadk+ 0.5 '(-1.0 1.0 -1.0 1.0))))
      (etouq
       (cons 'tex
	     (axis-aligned-quads:duaq 1 nil '(0.0 1.0 0.0 1.0)))))
     (incf len 4)
     )
    (make-instance
     'glhelp::gl-list
     :handle
     (glhelp:with-gl-list
       (gl:with-primitives :quads
	 (scratch-buffer:flush-my-iterator a
	   (scratch-buffer:flush-my-iterator b
	     ((lambda (times a b)
		(bind-iterator-in
		 (xyz single-float) a
		 (bind-iterator-in
		  (tex single-float) b
		  (dotimes (x times)
		    (%gl:vertex-attrib-2f 2 (tex) (tex))
		    (%gl:vertex-attrib-4f 0 (xyz) (xyz) (xyz) 1.0)))))
	      len a b))))))))

;;;;4 shades each of r g b a 0.0 1/3 2/3 and 1.0
(defun color-fun (color)
  (let ((one-third (etouq (coerce 1/3 'single-float))))
    (macrolet ((k (num)
		 `(* one-third (floatify (ldb (byte 2 ,num) color)))))
      (values (k 0)
	      (k 2)
	      (k 4)
	      (k 6)))))
(defun color-rgba (r g b a)
  (dpb a (byte 2 6)
       (dpb b (byte 2 4)
	    (dpb g (byte 2 2)
		 (dpb r (byte 2 0) 0)))))

(defmacro with-foreign-array ((var lisp-array type &optional (len (gensym)))
			      &rest body)
  (with-gensyms (i)
    (once-only (lisp-array)
      `(let ((,len (array-total-size ,lisp-array)))
	 (cffi:with-foreign-object (,var ,type ,len)
	   (dotimes (,i ,len)
	     (setf (cffi:mem-aref ,var ,type ,i)
		   (row-major-aref ,lisp-array ,i)))
	   ,@body)))))
(defparameter *16x16-tilemap* (rectangular-tilemap:regular-enumeration 16 16))

;;;each glyph gets a float which is a number that converts to 0 -> 255.
;;;this is an instruction that indexes into an "instruction set" thats the *attribute-bits*
(defparameter *attribute-bits*
  (let ((array (make-array (* 4 256) :element-type 'single-float)))
    (flet ((logbitter (index integer)
	     (if (logtest index integer)
		 1.0
		 0.0)))
      (dotimes (base 256)
	(let ((offset (* base 4)))
	  (setf (aref array (+ offset 0)) (logbitter 1 offset)
		(aref array (+ offset 1)) (logbitter 2 offset)
		(aref array (+ offset 2)) (logbitter 4 offset)
		(aref array (+ offset 3)) (logbitter 8 offset)))))
    array))
(defparameter *terminal256color-lookup* (make-array (* 4 256) :element-type 'single-float))
(defun write-to-color-lookup (color-fun)
  (let ((arr *terminal256color-lookup*))
    (dotimes (x 256)
      (let ((offset (* 4 x)))
	(multiple-value-bind (r g b a) (funcall color-fun x) 
	  (setf (aref arr (+ offset 0)) r)
	  (setf (aref arr (+ offset 1)) g)
	  (setf (aref arr (+ offset 2)) b)
	  (setf (aref arr (+ offset 3)) (if a a 1.0)))))
    arr))
(write-to-color-lookup 'color-fun)
(defun change-color-lookup (color-fun)
  (application::refresh 'color-lookup)
  (write-to-color-lookup color-fun))
(deflazy color-lookup (text-shader gl-context)
  (glhelp::use-gl-program text-shader)
  (glhelp:with-uniforms uniform text-shader
    (with-foreign-array (var *terminal256color-lookup* :float len)
      (%gl:uniform-4fv (uniform 'color-data)
		       (/ len 4)
		       var))))
(deflazy text-shader (text-shader-source gl-context) 
  (let ((shader (glhelp::create-gl-program text-shader-source)))
    (glhelp::use-gl-program shader)
    (glhelp:with-uniforms uniform shader
      (with-foreign-array (var *16x16-tilemap* :float len)
	(%gl:uniform-4fv (uniform 'font-data)
			 (/ len 4)
			 var))      
      (with-foreign-array (var *attribute-bits* :float len)
	(%gl:uniform-4fv (uniform 'attributedata)
			 (/ len 4)
			 var)))
    shader))

(deflazy flat-shader-source ()
  (glslgen:ashader
   :vs
   (glslgen2::make-shader-stage
    :out '((value-out "vec4"))
    :in '((position "vec4")
	  (value "vec4")
	  (projection-model-view "mat4"))
    :program
    '(defun "main" void ()
      (= "gl_Position" (* projection-model-view position))
      (= value-out value)))
   :frag
   (glslgen2::make-shader-stage
    :in '((value "vec4"))
    :program
    '(defun "main" void ()	 
      (= :gl-frag-color value)))
   :attributes
   '((position . 0) 
     (value . 3))
   :varyings
   '((value-out . value))
   :uniforms
   '((:pmv (:vertex-shader projection-model-view)))))
(deflazy flat-shader (flat-shader-source gl-context)
  (glhelp::create-gl-program flat-shader-source))

;;;;;;;;;;;;;;;;;;;;
(deflazy indirection-shader-source ()
  (glslgen:ashader
   :vs
   (glslgen2::make-shader-stage
    :out '((texcoord-out "vec2"))
    :in '((position "vec4")
	  (texcoord "vec2")
	  (projection-model-view "mat4"))
    :program
    '(defun "main" void ()
      (= "gl_Position" (* projection-model-view position))
      (= texcoord-out texcoord)))
   :frag
   (glslgen2::make-shader-stage
    :in '((texcoord "vec2")
	  (size "vec2"))
    :program
    '(defun "main" void ()
      ;;rg = fraction
      ;;ba = text lookup
      (/**/ vec2 foo)
      (= foo (/ (floor (* texcoord size))
	      (vec2 255.0)))	 
      (/**/ vec2 bar)
      (= bar
       (fract
	(* 
	 texcoord
	 size)))         
      (/**/ vec4 pixcolor) ;;font lookup
      (= (|.| pixcolor "rg") bar)       ;;fraction
      (= (|.| pixcolor "ba") foo)      ;;text lookup 
      (= :gl-frag-color pixcolor)))
   :attributes
   '((position . 0) 
     (texcoord . 2))
   :varyings
   '((texcoord-out . texcoord))
   :uniforms
   '((:pmv (:vertex-shader projection-model-view))
     (size (:fragment-shader size)))))
(deflazy indirection-shader (indirection-shader-source gl-context)
  (glhelp::create-gl-program indirection-shader-source))

;;;;;;;;;;;;;;;;
(defparameter *block-height* 16.0)
(defparameter *block-width* 8.0)
(defparameter *indirection-width* 0)
(defparameter *indirection-height* 0)
(deflazy indirection (gl-context)
  (glhelp::make-gl-framebuffer
   *indirection-width*
   *indirection-height*))
;;;Round up to next power of two
(defun power-of-2-ceiling (n)
  (ash 1 (ceiling (log n 2))))
(deflazy render-normal-text-indirection ((w application::w) (h application::h) gl-context)
  (let ((upw (power-of-2-ceiling w))
	(uph (power-of-2-ceiling h))
	(refract (getfnc 'indirection-shader)))
    (glhelp::use-gl-program refract)
    (glhelp:with-uniforms uniform refract
      (gl:uniform-matrix-4fv
       (uniform :pmv)
       (load-time-value (nsb-cga:identity-matrix))
       nil)
      (gl:uniformf (uniform 'size)
		   (/ w *block-width*)
		   (/ h *block-height*)))
    (gl:disable :cull-face)
    (gl:disable :depth-test)
    (gl:disable :blend)
    (glhelp:set-render-area 0 0 upw uph)
    (when (not (and (= *indirection-width* upw)
		    (= *indirection-height* uph)))
      (setf *indirection-width* upw
	    *indirection-height* uph)
      (application::refresh 'indirection t))
    (gl:bind-framebuffer :framebuffer (glhelp::handle (getfnc 'indirection)))
    (gl:clear :color-buffer-bit)
    (gl:clear :depth-buffer-bit)
    (gl:call-list (glhelp::handle (getfnc 'fullscreen-quad)))))
