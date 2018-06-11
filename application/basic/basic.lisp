(defpackage #:basic
  (:use #:cl #:utility #:application #:opengl-immediate
	#:sprite-chain #:point #:rectangle))
(in-package :basic)

(defparameter *ticks* 0)
(defparameter *saved-session* nil)
(defun per-frame ()
  (on-session-change *saved-session*
    (init))
  (incf *ticks*)
  (app))

(defparameter *app* nil)
(defun start ()
  (application:main
   (lambda ()
     (loop
	(application:poll-app)
	(if *app*
	    (testbed::per-frame)
	    (per-frame))
	(when (window:skey-j-p (window::keyval #\h))
	  (toggle *app*))))
   :width (* 80 8)
   :height (* 25 16)
   :title "a basic app"))
(defvar *this-directory* (filesystem-util:this-directory))

(defclass sprite ()
  ((bounding-box :accessor sprite.bounding-box
		 :initform (make-instance 'rectangle
					  :x0 -0.25 :y0 -0.25
					  :x1 0.25 :y1 0.25)
		 :initarg :bounding-box)
   (texture-section :accessor sprite.texture-section
		    :initform (make-instance 'rectangle
					     :x0 0.0 :y0 0.0
					     :x1 1.0 :y1 1.0)
		    :initarg :texture-section)
   (absolute-rectangle :accessor sprite.absolute-rectangle
		       :initform (make-instance 'rectangle)
		       :initarg :absolute-rectangle)
   (texture :accessor sprite.texture
	    :initform nil
	    :initarg :texture)
   (color :accessor sprite.color
	  :initform '(1.0 1.0 1.0 1.0)
	  :initarg :color)
   (position :accessor sprite.position
	     :initform (make-instance 'point)
	     :initarg :position)))

(defparameter *ndc-mouse-x* 0.0)
(defparameter *ndc-mouse-y* 0.0)

(dotimes (x 10)
  (add-sprite
   (make-instance
    'sprite
    :texture 'cons-texture
    :position (make-instance 'point :x (1- (random 2.0)) :y (1- (random 2.0)))
    :bounding-box (make-instance 'rectangle
				 :x0 0.0 :y0 0.0
				 :x1 (+ 0.1 (random 0.5)) :y1 (+ 0.1 (random 0.5))))))

(defparameter *pen-color* (list 1.0 0.0 0.0 1.0))
(defparameter *selection* nil)
(defparameter *drag-offset-x* 0.0)
(defparameter *drag-offset-y* 0.0)

(defun init ()
  (text-sub::change-color-lookup 'terminal-test::color-fun))
(defun app ()
  (setf *ndc-mouse-x* (+ -1 (* 2.0 (/ (floatify window::*mouse-x*)
				      window:*width*)))
	*ndc-mouse-y* (- 1 (* 2.0 (/ (floatify window::*mouse-y*)
				     window:*height*))))
  (when (window::skey-j-p (window::keyval #\esc))
    (application::quit))
;  #+nil
  (progn
    (when (window::skey-j-p (window::keyval :f2))
      (terminal-test::reset-term)
      )
    (when (window::skey-j-p (window::keyval :f1))
      (terminal-test::kill)
      ))
					;  #+nil
  (progn
    (multiple-value-bind (buf existsp) (control:get-input-characters)
      (when existsp
	(terminal-test::enter buf)))
    (terminal-test::update-terminal-stuff))
;  #+nil
  (when (window::skey-j-p (window::keyval #\A))
    (music::reset-listener)
    (music::play-at (merge-pathnames "wilhelm_scream.wav" *this-directory*) 0.0 0.0 0.0 1.0 1.0))
  (glhelp:set-render-area 0 0 window:*width* window:*height*)
  (gl:clear-color 0.5 0.5 0.5 0.0)
  (gl:clear :color-buffer-bit)
  (gl:polygon-mode :front-and-back :fill)
  (gl:disable :cull-face)
  (gl:disable :blend)
;  #+nil
  (render-stuff)
;  #+nil
  (foo0)
;  #+nil
  (let ((mousex *ndc-mouse-x*)
	(mousey *ndc-mouse-y*))
    (let ((program (getfnc 'flat-texture-shader)))
    (glhelp::use-gl-program program)
    (glhelp:with-uniforms uniform program
      (gl:uniformi (uniform 'sampler) 0)
      (glhelp::set-active-texture 0))

    #+nil
    (when (window::skey-p (window::mouseval :1))
      (deflazy cons-png ()
	(image-utility:flip-image
	 (color-test::test42 (floatify window::*scroll-y*)
					;	   (+ 0.5 (* 0.1 (sin *ticks*)))
			     (/ (+ 1 mousex) 2)
					;	   (+ 0.5 (* 0.1 (cos *ticks*)))
			     (/ (+ 1 mousey) 2)
			     )))
					;(sleep 0.1)
      )
    #+nil
    (render-sprite
     (load-time-value
      (make-instance
       'sprite
       :texture 'cons-texture
       :position (make-instance 'point :x -1.0 :y -1.0)
       :bounding-box (make-instance 'rectangle
				    :x0 0.0 :y0 0.0
				    :x1 2.0 :y1 2.0)))))
    
					;    #+nil
    (when (window::skey-j-p (window::mouseval :left))
      ;;search for topmost sprite to drag
      (let
	  ((sprite
	    (block cya
	      (do-sprite-chain (sprite) ()
		(with-slots (absolute-rectangle) sprite
		  (when (coordinate-inside-rectangle-p mousex mousey absolute-rectangle)
		    (return-from cya sprite)))))))
	(when sprite
	  (with-slots (position) sprite
	    (with-slots (x y) position
	      (setf *drag-offset-x* (- x mousex)
		    *drag-offset-y* (- y mousey))))
	  (setf *selection* sprite)
	  (topify-sprite sprite))))
					;    #+nil
    (typecase *selection*
      (sprite (with-slots (x y) (slot-value *selection* 'position)
		(setf x (+ *drag-offset-x* mousex)
		      y (+ *drag-offset-y* mousey))))))
;  #+nil
  (when (window::skey-j-r (window::mouseval :left))
    (setf *selection* nil))
;  #+nil
  (let ((program (getfnc 'flat-texture-shader)))
    (glhelp::use-gl-program program)
    (glhelp:with-uniforms uniform program
      (gl:uniformi (uniform 'sampler) 0)
      (glhelp::set-active-texture 0))
    (do-sprite-chain (sprite t) ()
      (render-sprite sprite))))

(defun render-tile (char-code x y background-color foreground-color)
  (color (byte/255 char-code)
	 (byte/255 background-color)
	 (byte/255 foreground-color))
  (vertex
   (floatify x)
   (floatify y)))


(defun foo0 ()
  (gl:line-width 10.0)
  (gl:point-size 10.0)
  (let ((program (getfnc 'flat-shader)))
    (glhelp::use-gl-program program))
  (with-primitives :line-loop
      'mesh-vertex-color
    (color 1.0 0.0 0.0)
    (vertex -1.0 -1.0)
    (color 0.0 1.0 0.0)
    (vertex *ndc-mouse-x* *ndc-mouse-y*)
    (color 0.0 0.0 1.0)
    (vertex 1.0 -1.0))
  (let ((foo (sin (/ *ticks* 60.0))))
    (with-primitives :line-loop
	'mesh-vertex-color
      (draw-quad *ndc-mouse-x* *ndc-mouse-y*
		 0.0 (+ foo 0.5)))
    (let ((*pen-color* '(0.0 0.0 0.0 1.0)))
      (gl:line-width 2.0)
      (with-primitives :triangle-fan
	  'mesh-vertex-color
	(draw-circle *ndc-mouse-x* *ndc-mouse-y* (* foo 0.1) 32 0.0)))
    (let ((program (getfnc 'flat-texture-shader)))
      (glhelp::use-gl-program program)
      (glhelp:with-uniforms uniform program
	(gl:disable :blend)
	(gl:uniformi (uniform 'sampler) 0)
	(glhelp::set-active-texture 0)
	(gl:bind-texture :texture-2d
			 (glhelp::handle (getfnc 'cons-texture))))
      (let ((*pen-color* '(1.0 0.0 1.0 1.0)))
	(with-primitives :quads
	    'mesh-vertex-tex-coord-color
	  (draw-textured-quad 0.5 0.5 0.9 0.9 0.0 0.0 1.0 1.0))))))

;;;more geometry
(defun draw-textured-quad (x0 y0 x1 y1 s0 t0 s1 t1)
  (destructuring-bind (r g b a) *pen-color*
    (color r g b a)
    (tex-coord s0 t0)
    (vertex x0 y0)

    (color r g b a)
    (tex-coord s0 t1)
    (vertex x0 y1)   

    (color r g b a)
    (tex-coord s1 t1)
    (vertex x1 y1)

    (color r g b a)
    (tex-coord s1 t0)
    (vertex x1 y0)
   ))
(defun draw-quad (x0 y0 x1 y1)
  (destructuring-bind (r g b a) *pen-color*
    (color r g b a)
    (vertex x0 y0)
    (color r g b a)
    (vertex x0 y1)
    (color r g b a)
    (vertex x1 y1)
    (color r g b a)
    (vertex x1 y0)))
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
	    (color r g b a)
	    (vertex point-x point-y))
	  (complex-multiply offset offseti d di offset offseti))))))
;;;

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
  (deflazy flat-shader (flat-shader-source gl-context)
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
       (tex . 2)
       (value . 3))
     :varyings
     '((value-out . value)
       (tex-out . tex))
     :uniforms
     '((sampler (:fragment-shader sampler)))))
  (deflazy flat-texture-shader (flat-texture-shader-source gl-context)
    (glhelp::create-gl-program flat-texture-shader-source)))

(deflazy cons-png ()
  (image-utility:read-png-file
   (filesystem-util:rebase-path #P"cons-cell.png" *this-directory*)
   t))
(deflazy cons-texture (cons-png gl-context)
  (make-instance
   'glhelp::gl-texture
   :handle
   (prog1
       (glhelp:pic-texture cons-png)
     (glhelp:apply-tex-params
      (quote ((:texture-min-filter . :linear)
	      (:texture-mag-filter . :linear)
	      (:texture-wrap-s . :clamp)
	      (:texture-wrap-t . :clamp)))))))

(defun render-sprite (sprite)
  (with-slots (texture-section bounding-box position color texture absolute-rectangle)
      sprite
      (flet ((render-stuff ()
	       (with-slots ((s0 x0) (t0 y0) (s1 x1) (t1 y1)) texture-section
		 (with-slots (x0 y0 x1 y1) bounding-box
		   (with-slots ((xpos x) (ypos y)) position
		     (let ((px0 (+ x0 xpos))
			   (py0 (+ y0 ypos))
			   (px1 (+ x1 xpos))
			   (py1 (+ y1 ypos)))
		       (with-slots (x0 y0 x1 y1) absolute-rectangle
			 (setf x0 px0 y0 py0 x1 px1 y1 py1))
		       (draw-textured-quad px0 py0 
					   px1 py1
					   s0 t0
					   s1 t1)))))))
	(if color
	    (let ((*pen-color* color))
	      (render-stuff))
	    (render-stuff))
	(when texture
	  (gl:bind-texture :texture-2d
			   (glhelp::handle (getfnc texture))))
	(gl:with-primitive :quads
	  (mesh-vertex-tex-coord-color)))
      ))

(progn
  (defparameter *numbuf* (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character))
  (defun render-stuff ()
    (text-sub::with-data-shader (uniform rebase)
      (gl:clear :color-buffer-bit)
      (gl:disable :depth-test)
      (setf (fill-pointer *numbuf*) 0)
      (with-output-to-string (stream *numbuf* :element-type 'character)
	(princ (get-internal-real-time) stream)
	(print "Hello World" stream)
	*numbuf*)
      (rebase -128.0 -128.0)
      (gl:point-size 1.0)
      (let ((count 0))
	(dotimes (x 16)
	  (dotimes (y 16)
	    (render-tile count x y count (- 255 count))
	    (incf count))))
      (let ((bgcol (byte/255
		    15
		    ;(text-sub::color-rgba 3 3 3 3)
		    ))
	    (fgcol (byte/255
		    0
		    ;(text-sub::color-rgba 0 0 0 3)
		    )))
	((lambda (x y string)
	   (let ((start x))
	     (let ((len (length string)))
	       (dotimes (index len)
		 (let ((char (aref string index)))
		   (cond ((char= char #\Newline)
			  (setf x start)
			  (decf y))
			 (t
			  (color (byte/255 (char-code char))
				 bgcol
				 fgcol)
			  (vertex (floatify x)
				  (floatify y)
				  0.0)
			  
			  (setf x (1+ x))))))
	       len)))
	 10.0 10.0 *numbuf*)))
    (gl:with-primitives :points
      (opengl-immediate::mesh-vertex-color))
    
    (when terminal-test::*term*
      (render-terminal 0 24)
      (gl:with-primitives :points
	(mesh-vertex-color)))
    (text-sub::with-text-shader (uniform)
      (gl:uniform-matrix-4fv
       (uniform :pmv)
       (load-time-value (nsb-cga:identity-matrix))
       nil)   
      (glhelp::bind-default-framebuffer)
      (glhelp:set-render-area 0 0 (getfnc 'application::w) (getfnc 'application::h))
      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:call-list (glhelp::handle (getfnc 'text-sub::fullscreen-quad))))))


;;;terminal emulation
(defun render-terminal (x y &optional (term terminal-test::*term*))
  (with-slots ((cx 3bst::x) (cy 3bst::y)) (with-slots (3bst::cursor) term 3bst::cursor)
    (terminal-test::do-term-values (glyph col row)
      (let ((char (3bst:c glyph))
	    (bg (3bst:bg glyph))
	    (fg (3bst:fg glyph)))
	(when (and (= cx col)
		   (= cy row))
	  (rotatef bg fg))
	(render-tile
	 (char-code char)
	 (+ x col)
	 (- y row)
	 bg
	 fg)))))
