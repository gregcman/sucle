(defpackage #:funtext
  (:use #:cl
	#:funfair
	#:funland))
(in-package #:funtext)

(defvar *this-directory* (filesystem-util:this-directory))
(deflazy font-png ()
  (opticl:read-png-file
   (filesystem-util:rebase-path #P"font.png"
				*this-directory*)))

(deflazy terrain-png ()
  (opticl:read-png-file
   (filesystem-util:rebase-path 
    #P"terrain.png"
    *this-directory*)))
(deflazy font-texture (font-png)
  (prog1
      (make-instance
       'glhelp::gl-texture
       :handle
       (glhelp:pic-texture
	font-png
	:luminance
	))
    (glhelp:apply-tex-params
     (quote ((:texture-min-filter . :nearest)
	     (:texture-mag-filter . :nearest)
	     (:texture-wrap-s . :repeat)
	     (:texture-wrap-t . :repeat))))))

(defparameter *reloadables*
  '(shader-test
    text-shader
    refraction-shader-text
    refraction-shader
    flat-shader-text
    flat-shader
    text))

(defparameter *identity-mat*
  (cg-matrix:identity-matrix))
(defparameter *view* (make-instance 'funfair::render-area))
(defparameter *view256x256* (make-instance 'funfair::render-area
					   :width 256
					   :height 256))
(defparameter *block-height* nil)
(defparameter *block-width* nil)
(setf (values *block-width* *block-height*)
      (values 8.0 16.0))

(defparameter *mouse-x* 0.0)
(defparameter *mouse-y* 0.0)
(defun per-frame (session)
  (declare (ignorable session))

  (map nil #'funfair::reload-if-dirty *reloadables*)
  
  (setf (render-area-width *view*) window::*width*
	(render-area-height *view*) window::*height*)

  (setf *mouse-x* (floatify (/ window::*mouse-x* *block-width* 128.0))
	*mouse-y* (floatify (/ (- window::*height* window::*mouse-y*)
			       *block-height*
			       128.0)))
  (when (window::skey-p (window::keyval :g))
   ;; (terpri)
  ;;  (princ "scrambling text")
    (copy-array-buf))

  (when (window::skey-j-p (window::keyval :r))
   ;; (terpri)
  ;;  (princ "scrambling text")
    (reload 'text-shader))

  (when (window::skey-j-p (window::keyval :escape))
   ;; (terpri)
  ;;  (princ "scrambling text")
    (funfair::quit))
  
  (let ((program (getfnc 'flat-shader)))
    (glhelp::use-gl-program program)
    (gl:bind-framebuffer :framebuffer (glhelp::handle (getfnc 'text-data)))
    (gl:viewport 0 0
		;; 64 64
		 256 256
		 )
    (glhelp:with-uniforms uniform program
      (gl:uniform-matrix-4fv
       (uniform :pmv)
       (cg-matrix:translate* *mouse-x* *mouse-y* 0.0)
       nil))
    #+nil
    (progn
      (gl:clear-color 0.1 0.11 0.3 0.0)
      (gl:clear :color-buffer-bit))
    (gl:call-list (glhelp::handle (getfnc 'text)))
    )

  (let ((program (getfnc 'text-shader)))
    (glhelp::use-gl-program program)
    (glhelp:with-uniforms uniform program
      (gl:uniform-matrix-4fv
       (uniform :pmv)
       *identity-mat*
       nil)
      (progn
	(gl:uniformi (uniform 'indirection) 0)
	(glhelp::set-active-texture 0)
	(gl:bind-texture :texture-2d
	;;		 (getfnc 'font-texture)
			 (glhelp::texture (getfnc 'indirection))
			 ))
      (progn
	(gl:uniformi (uniform 'font-texture) 2)
	(glhelp::set-active-texture 2)
	(gl:bind-texture :texture-2d
					;(texture (getfnc 'text-data))
			 (glhelp::handle (getfnc 'font-texture))
			 ))

      (progn
	(gl:uniformi (uniform 'text-data) 1)
	(glhelp::set-active-texture 1)
	(gl:bind-texture :texture-2d
			 ;;(getfnc 'terrain-texture)
			 ;;	 (getfnc 'font-texture)
			 (glhelp::texture (getfnc 'text-data))
			 )))
    
    (gl:disable :cull-face)
    (set-render-area *view*)
    (glhelp::bind-default-framebuffer)
    (gl:call-list (glhelp::handle (getfnc 'fullscreen-quad)))))


(defun uppow2 (n)
  (ash 1 (ceiling (log n 2))))
;;up to next power of two
(defun render-normal-text-refraction (w h)
  (let ((upw (uppow2 w))
	(uph (uppow2 h))
	(refract (getfnc 'refraction-shader)))
    (glhelp::use-gl-program refract)
    (glhelp:with-uniforms uniform refract
      (gl:uniform-matrix-4fv
       (uniform :pmv)
       *identity-mat*
       nil)
      (gl:uniformf (uniform 'size)
		   (/ w
		      *block-width*)
		   (/ h
		      *block-height*)))
    (gl:disable :cull-face)
    (set-render-area (make-instance 'funfair::render-area
				    :width upw
				    :height uph))
    (let ((obj (getfnc 'indirection)))
      (glhelp::gl-delete* obj))
    (funfair::remove-stuff 'indirection)
    (gl:bind-framebuffer :framebuffer (glhelp::handle (getfnc 'indirection)))
    (gl:call-list (glhelp::handle (getfnc 'fullscreen-quad)))))

(deflazy indirection ()
  (glhelp::make-gl-framebuffer
   (uppow2 window::*width*)
   (uppow2 window::*height*)))

(deflazy text-data ()
  (glhelp::make-gl-framebuffer 256 256))

(defun use-text ()
  (setf *trampoline* 'per-frame)
  (setf *pre-trampoline-hooks* (list 'scrubgl2))
  (setf window::*resize-hook* 'render-normal-text-refraction))

(defmacro progeach (fun body)
  `(etouq
    (cons 'progn
	  (mapcar ,fun
		  ,body))))

(deflazy fullscreen-quad ()
  (let ((a (scratch-buffer:my-iterator))
	(b (scratch-buffer:my-iterator))
	(len 0))
    (iter-ator:bind-iterator-out
     (pos single-float) a
     (iter-ator:bind-iterator-out
      (tex single-float) b

      (progeach
       (lambda (x) (list 'pos x))
       (axis-aligned-quads:quadk+ 0.0 '(-1.0 1.0 -1.0 1.0)))
      (progeach
       (lambda (x) (list 'tex x))
       (axis-aligned-quads:duaq 1 nil '(0.0 1.0 0.0 1.0))))
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
		(iter-ator:bind-iterator-in
		 (xyz single-float) a
		 (iter-ator:bind-iterator-in
		  (tex single-float) b
		  (dotimes (x times)
		    (%gl:vertex-attrib-2f 2 (tex) (tex))
		    (%gl:vertex-attrib-4f 0 (xyz) (xyz) (xyz) 1.0)))))
	      len a b))))))))

(deflazy shader-test ()
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

	 ;;;refraction
      (/**/ vec4 ind)
      (= ind ("texture2D" indirection texcoord))

      ;;where text changes go
      (/**/ ivec4 chardata)
      (= chardata
       (ivec4 (* 255.0 ("texture2D" text-data
				    (|.| ind "ba")))))

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

      (= :gl-frag-color	  
       (mix
	([] color-atlas (|.| chardata "g"))
	([] color-atlas (|.| chardata "b"))
	pixcolor)
       )))
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
		      shader-test)
  (let ((shader (glhelp::create-gl-program shader-test)))
    (glhelp::use-gl-program shader)
    (glhelp:with-uniforms uniform shader
      (with-foreign-array (var code :float len)
	(%gl:uniform-4fv (uniform 'font-data)
			 (/ len 4)
			 var))
      (with-foreign-array (var color :float len)
	(%gl:uniform-4fv (uniform 'color-data)
			 (/ len 4)
			 var)))
    shader))

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


(defparameter *16x16-tilemap* (rectangular-tilemap:regular-enumeration 16 16))

;;
(deflazy glsl-code-lookup ()
  (let ((a (make-array (* 4 256) :element-type 'single-float)))
    (dotimes (x 256)
      (let ((offset (* 4 x))
	    (tilemap-lookup *16x16-tilemap*))
	(etouq
	 (with-vec-params
	     `((offset ,@(vec-slots :rectangle
				    '((x0 :x0) (y0 :y0) (x1 :x1) (y1 :y1)
				      ))))
	   '(tilemap-lookup)
	   '(progn
	     (setf
	      (aref a (+ offset 0)) x0
	      (aref a (+ offset 1)) y0
	      (aref a (+ offset 2)) x1
	      (aref a (+ offset 3)) y1)
	     )))))
    a))

;;VT100 terminal emulator color uniform
(deflazy terminal256color-lookup ()
 (let ((a (make-array (* 4 256) :element-type 'single-float)))
   (dotimes (x 256)
     (let ((offset (* 4 x)))
       (multiple-value-bind (r g b) (color-rgb x) 
	 (setf (aref a (+ offset 0)) r
	       (aref a (+ offset 1)) g
	       (aref a (+ offset 2)) b
	       (aref a (+ offset 3)) 1.0))))
   a))

;;VT100 terminal emulator colors
(defun color-rgb (color)
  (labels ((c (r g b)
	     (values (/ r 255.0) (/ g 255.0) (/ b 255.0)))
	   (c6 (x)
	     (let ((b (mod x 6))
		   (g (mod (floor x 6) 6))
		   (r (mod (floor x 36) 6)))
	       (values (/ r 5.0) (/ g 5.0) (/ b 5.0))))
	   (g (x)
	     (let ((gray (/ x 23.0)))
	       (values gray gray gray))))
    (case color
      (0 (c 0 0 0))
      (1 (c 205 0 0))
      (2 (c 0 205 0))
      (3 (c 205 205 0))
      (4 (c 0 0 238))
      (5 (c 205 0 205))
      (6 (c 0 205 205))
      (7 (c 229 229 229))
      (8 (c 127 127 127))
      (9 (c 255 0 0))
      (10 (c 0 255 0))
      (11 (c 255 255 0))
      (12 (c 92 92 255))
      (13 (c 255 0 255))
      (14 (c 0 255 255))
      (15 (c 255 255 255))
      (t (let ((c (- color 16)))
	   (if (< c 216)
	       (c6 c)
	       (g (- c 216))))))))

(deflazy refraction-shader-text ()
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

(deflazy refraction-shader (refraction-shader-text)
  (glhelp::create-gl-program refraction-shader-text))

(defparameter *foo*
  (let ((*print-case* :downcase))
    (write-to-string
     '(let ((width 256)
	    (height 256))
       (cffi:with-foreign-object (b :uint8 (etouq (* 256 256 4)))
	 (dobox ((xpos 0 width)
		 (ypos 0 height))		   
		(let ((offset (the fixnum (* 4 (the fixnum (+ xpos (the fixnum (* ypos width))))))))
		  (let ((num
			 (random most-positive-fixnum)))
		    (let ((zero-bits (ldb (byte 8 24) num)))
		      (when (zerop zero-bits))
		      (setf (cffi:mem-aref b :uint8 (+ offset '0)) (ldb (byte 8 16) num)
			    (cffi:mem-aref b :uint8 (+ offset 1)) (ldb (byte 8 8) num)
			    (cffi:mem-aref b :uint8 (+ offset 2)) (logand 255 num) 
			    (cffi:mem-aref b :uint8 (+ offset 3)) zero-bits))
		    )))
	 (gl:bind-texture :texture-2d (texture (getfnc 'text-data)))
	 (gl:tex-sub-image-2d :texture-2d 0 0 0 width height :bgra :unsigned-byte b))))))

(defun copy-array-buf ()
  (let ((width 256)
	(height 256))
    (cffi:with-foreign-object (b :uint8 (etouq (* 256 256 4)))
      (dobox ((ypos 0 height)
	      (xpos 0 width))
	     (let ((base (the fixnum (+ xpos (the fixnum (* ypos width))))))
	       (let ((offset (the fixnum (* 4 base))))
		 (let ((num
			(logior (char-code (aref *foo* (mod base 1024)))
				(ash 0 8)
				(ash 255 16))
			 ;;(random most-positive-fixnum)
			 #+nil
			 (get-char-num
			  (get-char (the fixnum (+ xpos xstart))
				    (the fixnum (+ ypos ystart))))))
		   (let ((zero-bits (ldb (byte 8 24) num)))
		     (when (zerop zero-bits))
		     (setf (cffi:mem-aref b :uint8 (+ offset 0)) (ldb (byte 8 16) num)
			   (cffi:mem-aref b :uint8 (+ offset 1)) (ldb (byte 8 8) num)
			   (cffi:mem-aref b :uint8 (+ offset 2)) (logand 255 num) 
			   (cffi:mem-aref b :uint8 (+ offset 3)) zero-bits))
		   ))))
      (progn
	(gl:bind-texture :texture-2d (glhelp::texture (getfnc 'text-data)))
	(gl:tex-sub-image-2d :texture-2d 0 0 0 width height :bgra :unsigned-byte b)))))

(defun floatify (x)
  (coerce x 'single-float))

(deflazy text ()
 (let ((position (scratch-buffer:my-iterator))
       (value (scratch-buffer:my-iterator))
       (len 0))
   (iter-ator:bind-iterator-out
    (pos single-float) position
    (iter-ator:bind-iterator-out
     (value single-float) value

     (incf len
	   ((lambda (x y string)
	      (let ((start x))
		(let ((len (length string)))
		  (dotimes (index len)
		    (let ((char (aref string index)))
		      (cond ((char= char #\Newline)
			     (setf x start y (1- y)))
			    (t
			     (pos (floatify (/ x 128.0))
			      )
			     (pos (floatify (/ y 128.0))
			      )
			     (pos 0.0)
			     (value (/ (floatify (char-code char))
				       255.0))
			     (value 0.1)
			     (value 0.99)
			     
			     (setf x (1+ x))))))
		  len)))
	    -128.0 -128.0 *foo*))))
   (make-instance
    'glhelp::gl-list
    :handle
    (glhelp:with-gl-list
      (gl:with-primitives :points
	(scratch-buffer:flush-my-iterator position
	  (scratch-buffer:flush-my-iterator value
	    ((lambda (times position value)
	       (iter-ator:bind-iterator-in
		(xyz single-float) position
		(iter-ator:bind-iterator-in
		 (value single-float) value
		 (dotimes (x times)
		   (%gl:vertex-attrib-4f 2 (value) (value) (value) 1.0)
		   (%gl:vertex-attrib-4f 0 (xyz) (xyz) (xyz) 1.0)))))
	     len position value))))))))

(deflazy flat-shader-text ()
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
     (value . 2))
   :varyings
   '((value-out . value))
   :uniforms
   '((:pmv (:vertex-shader projection-model-view)))))

(deflazy flat-shader (flat-shader-text)
  (glhelp::create-gl-program flat-shader-text))

;;;;usage:
#+nil
(progn
  (ql:quickload :text-funfair)
  (in-package :funtext)
  (use-text)
  (main))

;;distance field fonts?

