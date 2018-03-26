(defpackage #:text-sub
  (:use #:cl
	#:application
	#:utility))
(in-package #:text-sub)

(deflazy text-data ()
  (glhelp::make-gl-framebuffer 256 256))

(deflazy text-shader-source ()
  (glslgen:ashader
   :version 120
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
      (/**/ ivec3 chardata)
      (= chardata
       (ivec3 (* 255.0 raw)))

      ;;font atlass coordinates
      (/**/ vec4 fontdata)
      (= fontdata
       ([]
	font-atlas
	(|.| chardata "r")))

      ;;font lookup
      (/**/ vec4 pixcolor)
      (= pixcolor
       ("texture2D"
	font-texture
	(mix (|.| fontdata "xy")
	     (|.| fontdata "zw")
					;(vec2 0.5 0.5)
	     (|.| ind "rg")
	     )))
      
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
     (text-data (:fragment-shader text-data))
     (color-data (:fragment-shader color-atlas))
     (font-data (:fragment-shader font-atlas))
     (font-texture (:fragment-shader font-texture)))))

(defvar *this-directory* (filesystem-util:this-directory))
(deflazy font-png ()
  (let ((array
	 (opticl:read-png-file
	  (filesystem-util:rebase-path #P"font.png"
				       *this-directory*))))
    (destructuring-bind (w h) (array-dimensions array)
      (let ((new
	     (make-array (list w h 4) :element-type '(unsigned-byte 8))))
	(dobox ((width 0 w)
		(height 0 h))
	       (let ((value (aref array width height)))
		 (dotimes (i 4)
		   (setf (aref new width height i) value))))
	new))))
(deflazy font-texture (font-png)
  (prog1
      (make-instance
       'glhelp::gl-texture
       :handle
       (glhelp:pic-texture
	font-png
	:rgba
	))
    (glhelp:apply-tex-params
     (quote ((:texture-min-filter . :nearest)
	     (:texture-mag-filter . :nearest)
	     (:texture-wrap-s . :repeat)
	     (:texture-wrap-t . :repeat))))))
(defparameter *16x16-tilemap* (rectangular-tilemap:regular-enumeration 16 16))
(deflazy glsl-code-lookup ()
  (let ((a (make-array (* 4 256) :element-type 'single-float)))
    (dotimes (x 256)
      (let ((offset (* 4 x))
	    (tilemap-lookup *16x16-tilemap*))
	(etouq
	 (with-vec-params
	     '((offset x0 y0 x1 y1))
	   '(tilemap-lookup)
	   '(etouq
	     (with-vec-params
		 '((offset ax0 ay0 ax1 ay1))
	       '(a symbol-macrolet)
	       '(setf
		 ax0 x0
		 ay0 y0
		 ax1 x1
		 ay1 y1))
	     )))))
    a))

(defun floatify (x)
  (coerce x 'single-float))

(defun byte-float (x)
  (/ (floatify x)
     255.0))

(defun color-rgb (color)
  (let ((one-third (etouq (coerce 1/3 'single-float))))
    (macrolet ((k (num)
		 `(* one-third (floatify (ldb (byte 2 ,num) color)))))
      (values (k 0)
	      (k 2)
	      (k 4)
	      (k 6)))))

(defun color (r g b a)
  (dpb a (byte 2 6)
       (dpb b (byte 2 4)
	    (dpb g (byte 2 2)
		 (dpb r (byte 2 0) 0)))))

#+nil
(progn
  (gl:clear-color (byte-float (char-code #\a))
		  (byte-float (color 0 3 2 1))
		  (byte-float (color 3 1 0 3))
		  1.0)
  (gl:clear :color-buffer-bit))

(defun per-frame (session)
  (declare (ignorable session))
  (get-fresh 'render-normal-text-indirection)
  (render-stuff))

(defparameter *trans* (nsb-cga:scale* (/ 1.0 128.0) (/ 1.0 128.0) 1.0))
(defun retrans (x y &optional (trans *trans*))
  (setf (aref trans 12) (/ x 128.0)
	(aref trans 13) (/ y 128.0))
  trans)

(defparameter *numbuf* (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character))
(defun render-stuff ()
  (gl:bind-framebuffer :framebuffer (glhelp::handle (getfnc 'text-data)))
  (application::%set-render-area 0 0 256 256)
  (gl:clear :color-buffer-bit)
  (let ((program (getfnc 'flat-shader)))
    (glhelp::use-gl-program program)
    (glhelp:with-uniforms
	uniform program
      (labels ((rebase (x y)
		 (gl:uniform-matrix-4fv
		  (uniform :pmv)
		  (retrans x y)
		  nil)))
	;#+nil
	(flet ((pos (x y z)
		 (;%gl:vertex-attrib-4f 3
				       gl:vertex
				       x y z 1.0))
	       (value (x y z)
		 (;%gl:vertex-attrib-4f 0
		  gl:color 
		  x y z 1.0)))
	  (setf (fill-pointer *numbuf*) 0)
	  (with-output-to-string (stream *numbuf* :element-type 'character)
	    (print (get-internal-run-time) stream)
	    *numbuf*)
	  (rebase 10.0 10.0)
	  (gl:disable :depth-test)
	  (gl:with-primitives :points
	    (let ((bgcol (byte-float (color 3 3 3 3)))
		  (fgcol (byte-float (color 0 0 0 3))))
	      ((lambda (x y string)
		 (let ((start x))
		   (let ((len (length string)))
		     (dotimes (index len)
		       (let ((char (aref string index)))
			 (cond ((char= char #\Newline)
				(setf x start)
				(decf y))
			       (t
				(value (byte-float (char-code char))
				       bgcol
				       fgcol)
				(pos (floatify x)
				     (floatify y)
				     0.0)
				
				(setf x (1+ x))))))
		     len)))
	       -128.0 -128.0 *numbuf*)))))))
  (let ((program (getfnc 'text-shader)))
    (glhelp::use-gl-program program)
    (glhelp:with-uniforms uniform program
      (gl:uniform-matrix-4fv
       (uniform :pmv)
       (load-time-value (nsb-cga:identity-matrix))
       nil)
      (progn
	(gl:uniformi (uniform 'indirection) 0)
	(glhelp::set-active-texture 0)
	(gl:bind-texture :texture-2d
			 (glhelp::texture (getfnc 'indirection))
			 ))
      (progn
	(gl:uniformi (uniform 'font-texture) 2)
	(glhelp::set-active-texture 2)
	(gl:bind-texture :texture-2d
			 (glhelp::handle (getfnc 'font-texture))
			 ))

      (progn
	(gl:uniformi (uniform 'text-data) 1)
	(glhelp::set-active-texture 1)
	(gl:bind-texture :texture-2d
			 (glhelp::texture (getfnc 'text-data))
			 )))
    
    (glhelp::bind-default-framebuffer)
    (application::%set-render-area 0 0 (getfnc 'application::w) (getfnc 'application::h))
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:call-list (glhelp::handle (getfnc 'fullscreen-quad)))
    ))

(deflazy fullscreen-quad ()
  (let ((a (scratch-buffer:my-iterator))
	(b (scratch-buffer:my-iterator))
	(len 0))
    (iterator:bind-iterator-out
     (pos single-float) a
     (iterator:bind-iterator-out
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
		(iterator:bind-iterator-in
		 (xyz single-float) a
		 (iterator:bind-iterator-in
		  (tex single-float) b
		  (dotimes (x times)
		    (%gl:vertex-attrib-2f 2 (tex) (tex))
		    (%gl:vertex-attrib-4f 0 (xyz) (xyz) (xyz) 1.0)))))
	      len a b))))))))


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
(deflazy text-shader ((glsl-code-lookup code) (terminal256color-lookup color)
		      text-shader-source)
  (let ((shader (glhelp::create-gl-program text-shader-source)))
    (glhelp::use-gl-program shader)
    (glhelp:with-uniforms uniform shader
      (with-foreign-array (var code :float len)
	(%gl:uniform-4fv (uniform 'font-data)
			 (/ len 4)
			 var))
      (with-foreign-array (var color :float len)
	(%gl:uniform-4fv (uniform 'color-data)
			 (/ len 3)
			 var)))
    shader))

#+nil
(eval-always
  (defparameter *vec-types* (make-hash-table :test 'equalp))
  (defun vec-slots (type args &optional (lookup *vec-types*))
    (let ((type-hash (gethash type lookup)))
      (mapcar (lambda (x)
		(let ((a (first x))
		      (b (second x)))
		  (list (gethash b type-hash) a)))
	      args)))
  (defun register-vec-slots (type args &optional (lookup *vec-types*))
    (let ((new-hash (make-hash-table :test 'equalp)))
      (dolist (x args)
	(setf (gethash (first x) new-hash)
	      (second x)))
      (setf (gethash type lookup) new-hash)))

  (register-vec-slots :rectangle (quote ((:x0 0)
					 (:y0 1)
					 (:x1 2)
					 (:y1 3))))

  (register-vec-slots :point (quote ((:x 0)
				     (:y 1)))))

;;color uniform
(deflazy terminal256color-lookup ()
 (let ((arr (make-array (* 4 256) :element-type 'single-float)))
   (dotimes (x 256)
     (let ((offset (* 4 x)))
       (multiple-value-bind (r g b a) (color-rgb x) 
	 (setf (aref arr (+ offset 0)) r
	       (aref arr (+ offset 1)) g
	       (aref arr (+ offset 2)) b
	       (aref arr (+ offset 3)) a))))
   arr))

;;;;;;;;;;;;;;;
(defun mesh-string-gl-points (x y string &optional
					   (bgcol (byte-float (color 3 3 3 3)))
					   (fgcol (byte-float (color 0 0 0 3))))
  (let ((position (scratch-buffer:my-iterator))
	(value (scratch-buffer:my-iterator))
	(len 0))
    (iterator:bind-iterator-out
     (pos single-float) position
     (iterator:bind-iterator-out
      (value single-float) value

      (incf len
	    ((lambda (x y string)
	       (let ((start x))
		 (let ((len (length string)))
		   (dotimes (index len)
		     (let ((char (aref string index)))
		       (cond ((char= char #\Newline)
			      (setf x start)
			      (decf y))
			     (t
			      (value (byte-float (char-code char))
				     bgcol
				     fgcol)
			      (pos (floatify x)
				   (floatify y)
				   0.0)
			      (setf x (1+ x))))))
		   len)))
	     x y string))))
    (glhelp:with-gl-list
      (gl:with-primitives :points
	((lambda (position value len)
	   (scratch-buffer:flush-my-iterator position
	     (scratch-buffer:flush-my-iterator value
	       ((lambda (times position value)
		  (iterator:bind-iterator-in
		   (xyz single-float) position
		   (iterator:bind-iterator-in
		    (value single-float) value
		    (dotimes (x times)
		      (%gl:vertex-attrib-4f 3 (value) (value) (value) 1.0)
		      (%gl:vertex-attrib-4f 0 (xyz) (xyz) (xyz) 1.0)))))
		len position value)))) position value len)))))

(deflazy flat-shader-source ()
  (glslgen:ashader
   :version 120
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
   '((:pmv (:vertex-shader projection-model-view)))))
(deflazy flat-shader (flat-shader-source)
  (glhelp::create-gl-program flat-shader-source))

;;;;;;;;;;;;;;;;;;;;
(deflazy indirection-shader-source ()
  (glslgen:ashader
   :version 120
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

      ;;
      (/**/ vec2 foo)
      (= foo (/ (floor (* texcoord size))
	      (vec2 255.0)))	 
      (/**/ vec2 bar)
      (= bar
       (fract
	(* 
	 texcoord
	 size)))
      
      ;;font lookup
      (/**/ vec4 pixcolor)

      ;;fraction
      (= (|.| pixcolor "rg") bar
       )
      ;;text lookup
      (= (|.| pixcolor "ba") foo
       )
      
      (= :gl-frag-color
       pixcolor
       )))
   :attributes
   '((position . 0) 
     (texcoord . 2))
   :varyings
   '((texcoord-out . texcoord))
   :uniforms
   '((:pmv (:vertex-shader projection-model-view))
     (size (:fragment-shader size)))))
(deflazy indirection-shader (indirection-shader-source)
  (glhelp::create-gl-program indirection-shader-source))

;;;;;;;;;;;;;;;;
(defparameter *block-height* 16.0)
(defparameter *block-width* 8.0)
(defparameter *indirection-width* 0)
(defparameter *indirection-height* 0)
(deflazy indirection ()
  (glhelp::make-gl-framebuffer
   *indirection-width*
   *indirection-height*))
;;;Round up to next power of two
(defun power-of-2-ceiling (n)
  (ash 1 (ceiling (log n 2))))
(deflazy render-normal-text-indirection ((application::w w) (application::h h))
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
		   (/ w
		      *block-width*)
		   (/ h
		      *block-height*)))
    (gl:disable :cull-face)
    (gl:disable :depth-test)
    (gl:disable :blend
     )
    (application::%set-render-area 0 0 upw uph)
    (when (not (and (= *indirection-width* upw)
		    (= *indirection-height* uph)))
      (setf *indirection-width* upw
	    *indirection-height* uph)
      (application::reload 'indirection))
    (gl:bind-framebuffer :framebuffer (glhelp::handle (getfnc 'indirection)))
    (gl:clear :color-buffer-bit)
    (gl:call-list (glhelp::handle (getfnc 'fullscreen-quad)))))
