(defpackage #:text-sub
  (:use #:cl
	#:application
	#:utility
	#:reverse-array-iterator-user))
(in-package #:text-sub)

;;FIXME:: 256 by 256 size limit for texture
(defparameter *text-data-height* 256)
(defparameter *text-data-width* 256)
(defparameter *text-data-what-type*
  ;;:framebuffer
  :texture-2d
  )
(defparameter *text-data-type* nil)
(glhelp:deflazy-gl text-data ()
  (setf *text-data-type* *text-data-what-type*)
  (let ((w *text-data-width*)
	(h *text-data-height*))
    (ecase *text-data-what-type*
      (:framebuffer
       (glhelp::make-gl-framebuffer w h))
      (:texture-2d
       (glhelp::wrap-opengl-texture
	(glhelp::create-texture nil w h))))))
(defun get-text-texture ()
  ;;;;FIXME:: getfnc must go before, because it has side effects.
  ;;;;are side effects and state unavoidable? a property of opengl?
  (let ((value (getfnc 'text-data)))
    (ecase *text-data-type*
      (:framebuffer (glhelp::texture value))
      (:texture-2d (glhelp::handle value)))))

(deflazy text-shader-source2 ()
  '(:vs
    "
out vec2 texcoord_out;
in vec4 position;
in vec2 texcoord;
uniform mat4 projection_model_view;
void main () {
gl_Position = projection_model_view * position;
texcoord_out = texcoord;
}"
    :frag
    "
in vec2 texcoord_out;
uniform sampler2D indirection;
uniform sampler2D text_data;
uniform vec4[400] color_font_info_atlas;
uniform sampler2D font_texture;
void main () {
vec4 ind = texture2D(indirection, texcoord_out); //indirection
vec4 raw = texture2D(text_data, ind.ba);
ivec4 chardata = ivec4(255.0 * raw); //where the text changes go
//convert a 4-bit number to a vec4 of 1.0's and 0.0's
vec4 infodata = color_font_info_atlas[384 + chardata.a];
vec2 offset = vec2(0.5, 0.5) * infodata.xy;
float opacity = infodata.z;
//font atlass coordinates
vec4 font_data = color_font_info_atlas[256 + chardata.r]; 
//bug workaround?
vec4 pixcolor = texture2D(font_texture,offset+vec2(0.5,1.0)*mix(font_data.xy,font_data.zw,ind.rg));
vec4 fin = mix(color_font_info_atlas[chardata.g],color_font_info_atlas[chardata.b],pixcolor);
gl_FragColor.rgb = fin.rgb;
gl_FragColor.a = opacity * fin.a;
}"
    :attributes
    (("position" . 0) 
     ("texcoord" . 2))
    :uniforms
    ((:pmv . "projection_model_view")
     (indirection . "indirection")
     ;;(attributedata (:fragment-shader attributeatlas))
     (text-data . "text_data")
     (color-font-info-data . "color_font_info_atlas")
     (font-texture . "font_texture"))))

(defvar *this-directory* (asdf:system-source-directory :text-subsystem))
(deflazy font-png ()
  (let ((array
	 (image-utility::load-image-from-file
	  (sucle-temp:path "res/font.png"))))
    (dobox ((width 0 (image-utility::image-width array))
	    (height 0 (image-utility::image-height array)))
	   (let ((value (aref array width height 0)))
	     (setf (aref array width height 3) 255)
	     (dotimes (i 3)
	       (setf (aref array width height i) value))))
    array))
(glhelp:deflazy-gl font-texture (font-png)
  (glhelp::wrap-opengl-texture (glhelp::create-opengl-texture-from-data font-png)))

(defparameter *trans* (nsb-cga:scale* (/ 1.0 128.0) (/ 1.0 128.0) 1.0))
(defun retrans (x y &optional (trans *trans*))
  (setf (aref trans 12) (/ x 128.0)
	(aref trans 13) (/ y 128.0))
  trans)
(defmacro with-data-shader ((uniform-fun rebase-fun) &body body)
  (with-gensyms (program)
    `(let ((,program (getfnc 'flat-shader)))
       (glhelp::use-gl-program ,program)
       (let ((framebuffer (getfnc 'text-data)))
	 (gl:bind-framebuffer :framebuffer (glhelp::handle framebuffer))
	 (glhelp:set-render-area 0 0
				 ;;TODO: not use generic functions?
				 (glhelp::x framebuffer)
				 (glhelp::y framebuffer)
				 ))
       (glhelp:with-uniforms ,uniform-fun ,program
	 (flet ((,rebase-fun (x y)
		  (gl:uniform-matrix-4fv
		   (,uniform-fun :pmv)
		   (retrans x y)
		   nil)))
	   ,@body)))))

;;;;FIXME:: managing opengl state blows
(defmacro with-text-shader ((uniform-fun) &body body)
  (with-gensyms (program)
    `(progn
         (getfnc 'render-normal-text-indirection)
	 (getfnc 'color-lookup)
	 (let ((,program (getfnc 'text-shader)))
	   (glhelp::use-gl-program ,program)
	   (glhelp:with-uniforms ,uniform-fun ,program
	     (glhelp::set-uniforms-to-textures
	      ((,uniform-fun 'indirection) (get-indirection-texture))
	      ((,uniform-fun 'font-texture) (glhelp::handle (getfnc 'font-texture)))
	      ((,uniform-fun 'text-data) (get-text-texture)))
	     ,@body)))))

(defun char-attribute (bold-p underline-p opaque-p)
  (logior
   (if bold-p
       2
       0)
   (if underline-p
       1
       0)
   (if opaque-p
       4
       0)))

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
#+nil
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
;;;256 color - 128 fontdata - 16 bit decoder
(defparameter *color-font-info-data*
  (let ((array (make-array (* 4 (+ 256 ;;color
				   128 ;;font
				   16  ;;bit decoder
				   )) :element-type 'single-float)))
    (dotimes (i (* 4 128))
      (setf (aref array (+ i (* 4 256)))
	    (aref *16x16-tilemap* i)))
    (flet ((fun (n)
	     (if n
		 1.0
		 0.0)))
      (dotimes (i 16)
	(let ((offset (* 4 (+ 256
			      128
			      i))))
	  (setf (aref array (+ offset 0)) (fun (logtest 1 i)))
	  (setf (aref array (+ offset 1)) (fun (logtest 2 i)))
	  (setf (aref array (+ offset 2)) (fun (logtest 4 i)))
	  (setf (aref array (+ offset 3)) (fun (logtest 8 i))))))
    array))
(defun write-to-color-lookup (color-fun)
  (let ((arr *color-font-info-data*))
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
(glhelp:deflazy-gl color-lookup (text-shader)
  (glhelp::use-gl-program text-shader)
  (glhelp:with-uniforms uniform text-shader
    (with-foreign-array (var *color-font-info-data* :float len)
      (%gl:uniform-4fv (uniform 'color-font-info-data)
		       (/ len 4)
		       var))))
(glhelp:deflazy-gl text-shader (text-shader-source2) 
  (let ((shader (glhelp::create-gl-program2 text-shader-source2)))
    (glhelp::use-gl-program shader)
    (glhelp:with-uniforms uniform shader
      (with-foreign-array (var *color-font-info-data* :float len)
	(%gl:uniform-4fv (uniform 'color-font-info-data)
			 (/ len 4)
			 var))
      #+nil
      (with-foreign-array (var *attribute-bits* :float len)
	(%gl:uniform-4fv (uniform 'attributedata)
			 (/ len 4)
			 var)))
    shader))

(glhelp:deflazy-gl flat-shader ()
  (glhelp::create-opengl-shader 
   "
out vec4 value_out;
in vec4 value;
in vec4 position;
uniform mat4 projection_model_view;

void main () {
gl_Position = projection_model_view * position;
value_out = value;
}"
   "
in vec4 value_out;
void main () {
gl_FragColor = value_out;
}"
   '(("position" 0) 
     ("value" 3))
   '((:pmv "projection_model_view"))))

;;;;;;;;;;;;;;;;
(defparameter *block-height* 16.0)
(defparameter *block-width* 8.0)
(defparameter *indirection-width* 0)
(defparameter *indirection-height* 0)
;;;;a framebuffer is faster and allows rendering to it if thats what you want
;;;;but a texture is easier to maintain. theres no -ext framebuffer madness,
;;;;no fullscreen quad, no shader. just an opengl texture and a char-grid
;;;;pattern to put in it.
(defparameter *indirection-what-type*
  :framebuffer
  ;;:texture-2d
  )
(defparameter *indirection-type* nil)
(glhelp:deflazy-gl indirection ()
  (setf *indirection-type* *indirection-what-type*)
  (ecase *indirection-what-type*
    (:framebuffer
     (glhelp::make-gl-framebuffer
		   *indirection-width*
		   *indirection-height*))
    (:texture-2d
     (glhelp::wrap-opengl-texture
      (glhelp::create-texture nil
			      *indirection-width*
			      *indirection-height*)))))
(defun get-indirection-texture ()
  (ecase *indirection-type*
    (:framebuffer (glhelp::texture (getfnc 'indirection)))
    (:texture-2d (glhelp::handle (getfnc 'indirection)))))

;;;Round up to next power of two
(defun power-of-2-ceiling (n)
  (ash 1 (ceiling (log n 2))))
(glhelp:deflazy-gl render-normal-text-indirection ((w application::w) (h application::h))
  (let* ((upw (power-of-2-ceiling w))
	 (uph (power-of-2-ceiling h))
	 (need-to-update-size
	  (not (and (= *indirection-width* upw)
		    (= *indirection-height* uph)))))
    (when need-to-update-size
      (setf *indirection-width* upw
	    *indirection-height* uph)
      (application::refresh 'indirection t))
    (getfnc 'indirection) ;;;refresh the indirection
    (ecase *indirection-type*
      (:framebuffer
       (let ((refract (getfnc 'indirection-shader)))
	 (glhelp::use-gl-program refract)
	 (glhelp:with-uniforms uniform refract
	   (gl:uniform-matrix-4fv
	    (uniform :pmv)
	    (load-time-value (nsb-cga:identity-matrix))
	    nil)
	   (gl:uniformf (uniform 'size)
			(/ w *block-width*)
			(/ h *block-height*))))
       (gl:disable :cull-face)
       (gl:disable :depth-test)
       (gl:disable :blend)
       (glhelp:set-render-area 0 0 upw uph)
       (gl:bind-framebuffer :framebuffer (glhelp::handle (getfnc 'indirection)))
       (gl:clear :color-buffer-bit)
       (gl:clear :depth-buffer-bit)
       (glhelp::slow-draw (getfnc 'fullscreen-quad)))
      (:texture-2d
       (gl:bind-texture :texture-2d (get-indirection-texture))
       (cffi:with-foreign-objects ((data :uint8 (* upw uph 4)))
	 (let* ((tempx (floatify (* upw *block-width*)))
		(tempy (floatify (* uph *block-height*)))
		(bazx (floatify (/ tempx w)))
		(bazy (floatify (/ tempy h)))
		(wfloat (floatify w))
		(hfloat (floatify h)))
	   (with-unsafe-speed
	     ;;FIXME:: nonportably declares things to be fixnums for speed
	     ;;The x and y components are independent of each other, so instead of
	     ;;computing x and y per point, compute once per x value or v value.
	     (dotimes (x (the fixnum upw))
	       (let* ((tex-x (+ 0.5 (floatify x)))
		      (barx (floor (* 255.0 (/ (mod tex-x bazx)
					       bazx))))
		      (foox (floor (/ (* wfloat tex-x)
				      tempx)))
		      (base (the fixnum (* 4 x)))
		      (delta (the fixnum (* 4 upw))))
		 (dotimes (y (the fixnum uph))
		   (setf (cffi:mem-ref data :uint8 (+ base 0)) barx
			 (cffi:mem-ref data :uint8 (+ base 2)) foox)
		   (setf base (the fixnum (+ base delta))))))
	     (dotimes (y (the fixnum uph))
	       (let* ((tex-y (+ 0.5 (floatify y)))			
		      (bary (floor (* 255.0 (/ (mod tex-y bazy)
					       bazy))))			
		      (fooy (floor (/ (* hfloat tex-y)
				      tempy)))
		      (base (the fixnum (* 4 (the fixnum (* upw y))))))
		 (dotimes (x upw)
		   (setf (cffi:mem-ref data :uint8 (+ base 1)) bary
			 (cffi:mem-ref data :uint8 (+ base 3)) fooy)
		   (setf base (the fixnum (+ base 4))))))))
	 (gl:tex-image-2d :texture-2d 0 :rgba upw uph 0 :rgba :unsigned-byte data))))))

;;;;;;;;;;;;;;;;;;;;
(glhelp:deflazy-gl indirection-shader ()
  (glhelp::create-opengl-shader
   "
out vec2 texcoord_out;
in vec4 position;
in vec2 texcoord;
uniform mat4 projection_model_view;

void main () {
gl_Position = projection_model_view * position;
texcoord_out = texcoord;
}"
   "
in vec2 texcoord_out;
uniform vec2 size;

void main () {
//rg = fraction
//ba = text lookup

vec2 foo = floor(texcoord_out * size) / vec2(255.0);
vec2 bar = fract(texcoord_out * size);
vec4 pixcolor; //font lookup
pixcolor.rg = bar; //fraction
pixcolor.ba = foo; // text lookup

gl_FragColor = pixcolor; 
}"
   '(("position" 0) 
     ("texcoord" 2))
   '((:pmv "projection_model_view")
     (size "size"))))

(glhelp:deflazy-gl fullscreen-quad ()
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
    (let ((buffer (make-array (* len (+ 2 3 1)))))
      (scratch-buffer:flush-my-iterator a
	(scratch-buffer:flush-my-iterator b
	  (let ((count 0))
	    (flet ((add (n)
		     (setf (aref buffer count) n)
		     (incf count)))
	      (bind-iterator-in
	       (xyz single-float) a
	       (bind-iterator-in
		(tex single-float) b
		(dotimes (x len)
		  (add (tex))
		  (add (tex))
		  (add (xyz))
		  (add (xyz))
		  (add (xyz))
		  (add 1.0))))))))
      (let ((count 0))
	(flet ((getn ()
		 (prog1 (aref buffer count)
		   (incf count))))
	  (ecase glhelp::*slow-draw-type*
	    (:display-list
	     (make-instance
	      'glhelp::gl-list
	      :handle
	      (glhelp:with-gl-list
	       (gl:with-primitives
		:quads
		(dotimes (x len)
		  (%gl:vertex-attrib-2f 2
					(getn)
					(getn))
		  (%gl:vertex-attrib-4f 0
					(getn)
					(getn)
					(getn)
					(getn)))))))
	    (:vertex-array-object
	     (glhelp::make-vertex-array
	      buffer
	      (quads-triangles-index-buffer len)
	      (glhelp::simple-vertex-array-layout
	       '((2 2)
		 (0 4)))
	      :triangles))))))))

(defun quads-triangles-index-buffer (n)
  ;;0->3 quad
  ;;0 1 2 triangle
  ;;0 2 3 triangle
  (let ((array (make-array (* 6 n))))
    (dotimes (i n)
      (let ((base (* i 6))
	    (quad-base (* i 4)))
	(flet ((foo (a b)
		 (setf (aref array (+ base a))
		       (+ quad-base b))))
	  (foo 0 0)
	  (foo 1 1)
	  (foo 2 2)
	  (foo 3 0)
	  (foo 4 2)
	  (foo 5 3))))
    array))

(defun draw-fullscreen-quad ()
  (glhelp::slow-draw (getfnc 'text-sub::fullscreen-quad)))
