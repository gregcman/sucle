(defpackage #:basic0
  (:use #:cl #:utility #:application #:opengl-immediate
	#:sprite-chain #:point #:rectangle))
(in-package :basic0)

(defparameter *ticks* 0)
(defparameter *saved-session* nil)
(defun per-frame ()
  (on-session-change *saved-session*
    (init))
  (incf *ticks*)
  (app))

(defparameter *glyph-height* 16.0)
(defparameter *glyph-width* 8.0)

(defparameter *app* nil)
(defun start ()
  (application:main
   (lambda ()
     (loop
	(application:poll-app)
	(if *app*
	    nil
	    (per-frame))
	(when (window:skey-j-p (window::keyval #\h))
	  (toggle *app*))))
   :width (floor (* 80 *glyph-width*))
   :height (floor (* 25 *glyph-height*))
   :title ""))
(defvar *this-directory* (filesystem-util:this-directory))

(defclass sprite ()
  ((bounding-box :accessor sprite.bounding-box
		 :initform (make-instance 'rectangle
					  :x0 -0.25 :y0 -0.25
					  :x1 0.25 :y1 0.25)
		 :initarg :bounding-box)
   (absolute-rectangle :accessor sprite.absolute-rectangle
		       :initform (make-instance 'rectangle)
		       :initarg :absolute-rectangle)
   (color :accessor sprite.color
	  :initform '(1.0 1.0 1.0 1.0)
	  :initarg :color)
   (string :accessor sprite.string
	   :initform "Hello World"
	   :initarg :string)
   (position :accessor sprite.position
	     :initform (make-instance 'point)
	     :initarg :position)))

(defun closest-multiple (x n)
  (* n (round x n)))

(defparameter *ndc-mouse-x* 0.0)
(defparameter *ndc-mouse-y* 0.0)

(progn
  (setf sprite-chain::*sprites* (sprite-chain:make-sprite-chain))
  (dotimes (x 10)
    (add-sprite
     (make-instance
      'sprite
      :position (make-instance 'point
			       :x (* *glyph-width* (random 20))
			       :y (* *glyph-height* (random 20)))
      :bounding-box (make-instance 'rectangle
				   :x0 0.0 :y0 0.0
				   :x1 (* *glyph-width* 1)
				   :y1 (* *glyph-height* 1))))))

(defparameter *pen-color* (list 1.0 0.0 0.0 1.0))
(defparameter *selection* nil)
(defparameter *drag-offset-x* 0.0)
(defparameter *drag-offset-y* 0.0)

(defun init ())
(defun app ()
  (setf *ndc-mouse-x* (floatify window::*mouse-x*)
	*ndc-mouse-y* (- window::*height* (floatify window::*mouse-y*)))
  (when (window::skey-j-p (window::keyval #\esc))
    (application::quit))

  (glhelp:set-render-area 0 0 window:*width* window:*height*)
  (gl:clear-color 0.5 0.5 0.5 0.0)
  (gl:clear :color-buffer-bit)
  (gl:polygon-mode :front-and-back :fill)
  (gl:disable :cull-face)
  (gl:disable :blend)
;  #+nil
  (render-stuff)
  (let ((mousex *ndc-mouse-x*)
	(mousey *ndc-mouse-y*))
    (let ((program (getfnc 'flat-shader)))
      (glhelp::use-gl-program program)
      (glhelp:with-uniforms uniform program
	(gl:uniform-matrix-4fv (uniform :pmv)
			       (nsb-cga:matrix*
				(nsb-cga:scale*
				 (/ 2.0 (floatify window::*width*))
				 (/ 2.0 (floatify window::*height*))
				 1.0)
				(nsb-cga:translate* 
				 (/ (floatify window::*width*)
				    -2.0)				 
				 (/ (floatify window::*height*)
				    -2.0)
				 0.0))
			       nil)))
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
		(setf x (closest-multiple (+ *drag-offset-x* mousex) 8.0)
		      y (closest-multiple (+ *drag-offset-y* mousey) 16.0))))))
;  #+nil
  (when (window::skey-j-r (window::mouseval :left))
    (setf *selection* nil))
  (do-sprite-chain (sprite t) ()
    (render-sprite sprite)))

(defun render-tile (char-code x y background-color foreground-color)
  (color (byte/255 char-code)
	 (byte/255 background-color)
	 (byte/255 foreground-color))
  (vertex
   (floatify x)
   (floatify y)))

;;;more geometry
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

(progn
  (deflazy flat-shader-source ()
    (glslgen:ashader
     :version 120
     :vs
     (glslgen2::make-shader-stage
      :out '((value-out "vec4"))
      :in '((position "vec4")
	    (value "vec4")
	    (pmv "mat4"))
      :program
      '(defun "main" void ()
	(= "gl_Position" (* pmv position))
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
     '((:pmv (:vertex-shader pmv)))))
  (deflazy flat-shader (flat-shader-source gl-context)
    (glhelp::create-gl-program flat-shader-source)))

(defun render-sprite (sprite)
  (with-slots (bounding-box position color absolute-rectangle)
      sprite
      (flet ((render-stuff ()
	       (with-slots (x0 y0 x1 y1) bounding-box
		 (with-slots ((xpos x) (ypos y)) position
		   (let ((px0 (+ x0 xpos))
			 (py0 (+ y0 ypos))
			 (px1 (+ x1 xpos))
			 (py1 (+ y1 ypos)))
		     (with-slots (x0 y0 x1 y1) absolute-rectangle
		       (setf x0 px0 y0 py0 x1 px1 y1 py1))
		     (draw-quad px0 py0 
				px1 py1))))))
	(if color
	    (let ((*pen-color* color))
	      (render-stuff))
	    (render-stuff))
	(gl:with-primitive :quads
	  (mesh-vertex-tex-coord-color)))
      ))

(defun draw-string
    (x y string &optional
		  (fgcol
		   (byte/255		    
		    (text-sub::color-rgba 0 0 0 3)
		    ))
		  (bgcol
		   (byte/255
		    (text-sub::color-rgba 3 3 3 3)
		    )))
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

(progn
  (defparameter *numbuf* (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character))
  (defun render-stuff ()
    (text-sub::with-data-shader (uniform rebase)
      (gl:clear :color-buffer-bit)
      (gl:disable :depth-test)

      ;;a rainbow
      (let ((count 0))
	(dotimes (x 16)
	  (dotimes (y 16)
	    (render-tile count x y count (- 255 count))
	    (incf count))))

      ;;mouse coordinates
      (setf (fill-pointer *numbuf*) 0)
      (with-output-to-string (stream *numbuf* :element-type 'character)
	(princ (list (floor *ndc-mouse-x*)
		     (floor *ndc-mouse-y*)) stream)
	*numbuf*)
      (draw-string 10.0 10.0 *numbuf*)

      ;;"sprites"
      (do-sprite-chain (sprite t) ()
	(with-slots (position string)
	    sprite
	  (with-slots ((xpos x) (ypos y)) position
	    (draw-string (+ 1.0 (/ xpos *glyph-width*))
			 (/ ypos *glyph-height*)
			 string))))
      
      (rebase -128.0 -128.0))
    (gl:point-size 1.0)
    (gl:with-primitives :points
      (opengl-immediate::mesh-vertex-color))
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
