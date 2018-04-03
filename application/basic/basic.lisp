(defpackage #:basic
  (:use #:cl #:utility #:application #:opengl-immediate))
(in-package :basic)

(defparameter *saved-session* nil)
(defun per-frame (&optional session)
  (declare (ignorable session))
  (unless (eq *saved-session* session)
    (setf *saved-session* session)
    (init))
  (app))
(defun start ()
  (let ((application::*argument-values*
	 (list nil
	       *window-start-width*
	       *window-start-height*
	       *window-start-title*)))
    (setf application::*trampoline*
	  '(per-frame))
    (application::main)))
(defvar *this-directory* (filesystem-util:this-directory))

(defparameter *window-start-height* 480)
(defparameter *window-start-width* 780)
(defparameter *window-start-title* "basic app")
(defparameter *ticks* 0)
(defparameter *ndc-mouse-x* 0.0)
(defparameter *ndc-mouse-y* 0.0)
(defparameter *sprites* nil)

(defparameter *pen-color* (list 1.0 0.0 0.0 1.0))
(defparameter *selection* nil)
(defparameter *drag-offset-x* 0.0)
(defparameter *drag-offset-y* 0.0)

(defun init ()
  (setf *sprites* (doubly-linked-list:circular "sentinel"))
  (text-sub::change-color-lookup 'terminal-test::color-fun)
  (let ((chain *sprites*))
    (dotimes (x 10)
      (doubly-linked-list:insert-next chain
	    (doubly-linked-list:make-node :payload
	     (make-instance
	      'sprite
	      :texture 'cons-texture
	      :position (make-instance 'point :x (1- (random 2.0)) :y (1- (random 2.0)))
	      :bounding-box (make-instance 'rectangle
					   :x0 0.0 :y0 0.0
					   :x1 (+ 0.1 (random 0.5)) :y1 (+ 0.1 (random 0.5)))))))))
(defun app ()
  (incf *ticks*)
  (setf *ndc-mouse-x* (+ -1 (* 2.0 (/ (floatify window::*mouse-x*)
				      window:*width*)))
	*ndc-mouse-y* (- 1 (* 2.0 (/ (floatify window::*mouse-y*)
				     window:*height*))))
  (when (window::skey-j-p (window::keyval #\esc))
    (application::quit))
  (when (window::skey-j-p (window::keyval :f2))
    (terminal-test::reset-term)
    )
  (when (window::skey-j-p (window::keyval :f1))
    (terminal-test::kill)
    )
  #+nil
  (when (window::skey-j-p (window::keyval :r))
    (application::reload 'cons-texture))
  #+nil
  (when (window::skey-j-p (window::keyval #\A))
    (music::reset-listener)
    (music::play-at (merge-pathnames "wilhelm_scream.wav" *this-directory*) 0.0 0.0 0.0 1.0 1.0))
  (application::%set-render-area 0 0 window:*width* window:*height*)
  (gl:clear-color 0.5 0.5 0.5 0.0)
  (gl:clear :color-buffer-bit)
  (gl:disable :cull-face)
  (gl:disable :blend)
  (more-test)
  (render-stuff)
  (foo0)
  (let ((mousex *ndc-mouse-x*)
	(mousey *ndc-mouse-y*))
    (when (window::skey-j-p (window::mouseval :left))
      ;;search for topmost sprite to drag
      (tagbody 
	 (let ((last *sprites*))
	   (do ((sprite-cell (doubly-linked-list:node-next last)
			     (doubly-linked-list:node-next sprite-cell)))
	       ((eq sprite-cell *sprites*))
	     (let ((sprite (doubly-linked-list:node-payload sprite-cell)))
	       (with-slots (absolute-rectangle position) sprite
		 (when (coordinate-inside-rectangle-p mousex mousey absolute-rectangle)
		   (with-slots (x y) position
		     (setf *drag-offset-x* (- x mousex)
			   *drag-offset-y* (- y mousey)))
		   (setf *selection* sprite)
		   (doubly-linked-list:detach sprite-cell)
		   (doubly-linked-list:insert-next *sprites* sprite-cell)
		   (go end))))
	     (setf last sprite-cell)))
	 end))
    (typecase *selection*
      (sprite (with-slots (x y) (slot-value *selection* 'position)
		(setf x (+ *drag-offset-x* mousex)
		      y (+ *drag-offset-y* mousey))))))
  (when (window::skey-j-r (window::mouseval :left))
    (setf *selection* nil))
  (let ((program (getfnc 'flat-texture-shader)))
    (glhelp::use-gl-program program)
    (glhelp:with-uniforms uniform program
      (gl:uniformi (uniform 'sampler) 0)
      (glhelp::set-active-texture 0))
    (do ((sprite-cell (doubly-linked-list:node-prev *sprites*)
		      (doubly-linked-list:node-prev sprite-cell)))
	((eq sprite-cell *sprites*))
      (let ((sprite (doubly-linked-list:node-payload sprite-cell)))
	(render-sprite sprite)))))

(defun render-terminal (x y &optional (term terminal-test::*term*))
  (flet ((value (r g b x y)
	   (color (byte/255 r)
		  (byte/255 g)
		  (byte/255 b))
	   (vertex
	    (floatify x)
	    (floatify y))))
    (with-slots ((cx 3bst::x) (cy 3bst::y)) (with-slots (3bst::cursor) term 3bst::cursor)
      (terminal-test::do-term-values (glyph col row)
	(let ((char (3bst:c glyph))
	      (bg (3bst:bg glyph))
	      (fg (3bst:fg glyph)))
	  (when (and (= cx col)
		     (= cy row))
	    (rotatef bg fg))
	  (value (char-code char)
		 bg
		 fg
		 (+ x col)
		 (- y row)))))))

(defmacro get-control-sequence ((control-state char-var shift control alt super) &body body)
  (once-only (control-state shift control alt super)
    `(let ((something-flag nil))
       (labels ((enter-string (string)
		  (let ((len (length string)))
		    (unless (zerop len)
		      (setf something-flag t)
		      (dotimes (index len)
			(enter-char (aref string index))))))
		(enter-char (,char-var)
		  ,@body)
		(enter (x)
		  (setf something-flag t)
		  (enter-char x)))
	 (macrolet ((foo (x a b)
		      `(when (window::skey-j-p-or-repeat (window::keyval ,a))
			 ,(list (ecase x
				  (0 'enter)
				  (1 'enter-string)) b))))
	   (foo 0 :enter #\return)
	   (foo 0 :backspace #\del)
	   (foo 0 :tab #\Tab)
	   (foo 1 :up "[A")
	   (foo 1 :down "[B")
	   (foo 1 :left "[D")
	   (foo 1 :right "[C"))      

	 (window::do-character-keys ((window::control-state-jp-or-repeat ,control-state) true? code)
	   (when true?
	     (multiple-value-bind (char esc)
		 (character-modifier-bits:character-modifier-bits
		  (char-code (char-downcase (code-char code)))
		  ,shift ,control ,alt ,super)
	       (when esc
		 (enter #\esc))
	       (enter (code-char char))))))
       something-flag)))
(defparameter *command-buffer* (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character))
(defun more-test ()
  (when (get-control-sequence (window::*control-state*
			       char
			       window::*shift*
			       window::*control*
			       window::*alt*
			       window::*super*)
	  (vector-push-extend char *command-buffer*))
    
    (terminal-test::enter *command-buffer*)
    (setf (fill-pointer *command-buffer*) 0))
  (terminal-test::update-terminal-stuff))



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
  (flip-image:flip-image
   (opticl:read-png-file
    (filesystem-util:rebase-path #P"cons-cell.png" *this-directory*))))
(deflazy cons-texture (cons-png gl-context)
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
	      (:texture-wrap-t . :clamp)))))))

(defclass rectangle ()
  ((x0 :accessor rectangle.x0
       :initform 0.0
       :initarg :x0)
   (y0 :accessor rectangle.y0
       :initform 0.0
       :initarg :y0)
   (x1 :accessor rectangle.x1
       :initform 0.0
       :initarg :x1)
   (y1 :accessor rectangle.y1
       :initform 0.0
       :initarg :y1)))

(defclass point ()
  ((x :accessor point.x
       :initform 0.0
       :initarg :x)
   (y :accessor point.y
       :initform 0.0
       :initarg :y)))

(defun coordinate-inside-rectangle-p (x y rectangle)
  (with-slots (x0 y0 x1 y1) rectangle
    (and (< x0 x x1)
	 (< y0 y y1))))

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
	*numbuf*)
      (rebase -128.0 -128.0)
      (gl:point-size 1.0)
      (let ((count 0))
	(dotimes (x 16)
	  (dotimes (y 16)
	    (let ((val (byte/255 count)))
	      (color val
		     val
		     val))
	    (vertex (floatify x)
		    (floatify y)
		    0.0)
	    (incf count))))
					;   (basic::render-terminal 0 24)
      (let ((bgcol (byte/255 (text-sub::color-rgba 3 3 3 3)))
	    (fgcol (byte/255 (text-sub::color-rgba 0 0 0 3))))
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
					; #+nil
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
      (application::%set-render-area 0 0 (getfnc 'application::w) (getfnc 'application::h))
      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:call-list (glhelp::handle (getfnc 'text-sub::fullscreen-quad))))))
