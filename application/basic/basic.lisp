(defpackage #:basic
  (:use #:cl #:utility #:application))
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
	  '(per-frame text-sub::per-frame
	    ))
    (application::main)))
(defvar *this-directory* (filesystem-util:this-directory))

(defmacro floatify (x)
  `(coerce ,x 'single-float))

(defparameter *window-start-height* 480)
(defparameter *window-start-width* 780)
(defparameter *window-start-title* "basic app")
(defparameter *ticks* 0)
(defparameter *ndc-mouse-x* 0.0)
(defparameter *ndc-mouse-y* 0.0)
(defparameter *sprites* nil)

(defparameter *reloadables*
  '(flat-shader-source
    flat-shader
    flat-texture-shader-source
    flat-texture-shader
    cons-png
    cons-texture))

(defparameter *pen-color* (list 1.0 0.0 0.0 1.0))
(defparameter *selection* nil)
(defparameter *drag-offset-x* 0.0)
(defparameter *drag-offset-y* 0.0)

(defun init ()
  (setf *sprites* (circular-dlink "sentinel"))
  (let ((chain *sprites*))
    (dotimes (x 10)
      (dlink-insert-right chain
	    (make-dlink :payload
	     (make-instance
	      'sprite
	      :texture (getfnc 'cons-texture)
	      :position (make-instance 'point :x (1- (random 2.0)) :y (1- (random 2.0)))
	      :bounding-box (make-instance 'rectangle
					   :x0 0.0 :y0 0.0
					   :x1 (+ 0.1 (random 0.5)) :y1 (+ 0.1 (random 0.5)))))))))
(defun app ()
  (incf *ticks*)
  (map nil #'application::reload-if-dirty *reloadables*)
  (setf *ndc-mouse-x* (+ -1 (* 2.0 (/ (floatify window::*mouse-x*)
				      window:*width*)))
	*ndc-mouse-y* (- 1 (* 2.0 (/ (floatify window::*mouse-y*)
				     window:*height*))))
  (when (window::skey-j-p (window::keyval #\Q))
    (application::quit))
  #+nil
  (when (window::skey-j-p (window::keyval :r))
    (application::reload 'cons-texture))
  (when (window::skey-j-p (window::keyval #\A))
    (music::reset-listener)
    (music::play-at (merge-pathnames "wilhelm_scream.wav" *this-directory*) 0.0 0.0 0.0 1.0 1.0))
  (application::%set-render-area 0 0 window:*width* window:*height*)
  (gl:clear-color 0.5 0.5 0.5 0.0)
  (gl:clear :color-buffer-bit)
  (gl:disable :cull-face)
  (gl:disable :blend)

  (let ((mousex *ndc-mouse-x*)
	(mousey *ndc-mouse-y*))
    (when (window::skey-j-p (window::mouseval :left))
      ;;search for topmost sprite to drag
      (tagbody 
	 (let ((last *sprites*))
	   (do ((sprite-cell (dlink-right last) (dlink-right sprite-cell)))
	       ((eq sprite-cell *sprites*))
	     (let ((sprite (dlink-payload sprite-cell)))
	       (with-slots (absolute-rectangle position) sprite
		 (when (coordinate-inside-rectangle-p mousex mousey absolute-rectangle)
		   (with-slots (x y) position
		     (setf *drag-offset-x* (- x mousex)
			   *drag-offset-y* (- y mousey)))
		   (setf *selection* sprite)
		   (dlink-remove sprite-cell)
		   (dlink-insert-right *sprites* sprite-cell)
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
    (do ((sprite-cell (dlink-left *sprites*) (dlink-left sprite-cell)))
	((eq sprite-cell *sprites*))
      (let ((sprite (dlink-payload sprite-cell)))
	(render-sprite sprite)))))
#+nil
(defun foo0 ()
  (gl:line-width 10.0)
  (gl:point-size 10.0)
  (let ((program (getfnc 'flat-shader)))
    (glhelp::use-gl-program program))
  (gl:with-primitive :line-loop
    (gl:color 1.0 0.0 0.0)
    (gl:vertex -1.0 -1.0)
    (gl:color 0.0 1.0 0.0)
    (gl:vertex *ndc-mouse-x* *ndc-mouse-y*)
    (gl:color 0.0 0.0 1.0)
    (gl:vertex 1.0 -1.0))
  (let ((foo (sin (/ *ticks* 60.0))))
    (gl:with-primitive :line-loop
      (draw-quad *ndc-mouse-x* *ndc-mouse-y*
		 0.0 (+ foo 0.5)))
    (let ((*pen-color* '(0.0 0.0 0.0 1.0)))
      (gl:line-width 2.0)
      (gl:with-primitive :triangle-fan
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
      (when texture
	(gl:bind-texture :texture-2d
			 (glhelp::handle texture)))
      (flet ((render-stuff ()
	       (gl:with-primitive :quads
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
					     s1 t1))))))))
	(if color
	    (let ((*pen-color* color))
	      (render-stuff))
	    (render-stuff)))
      ))

(defstruct dlink
  (left nil)
  (right nil)
  (payload nil))

(defun dlink-insert-right (link new)
  (let ((right (dlink-right link)))
    (dlink-link link new)
    (when (dlink-p right)
      (dlink-link new right))
    new))
(defun dlink-insert-left (new link)
  (let ((left (dlink-left link)))
    (dlink-link new link)
    (when (dlink-p left)
      (dlink-link left new))
    new))
(defun dlink-remove (link)
  (let ((left (dlink-left link))
	(right (dlink-right link)))
    (dlink-link left right)
    link))

(defun dlink-link (left right)
  (when (dlink-p left)
    (setf (dlink-right left) right))
  (when (dlink-p right)
    (setf (dlink-left right) left)))

(defun dlink-cons (item right)
  (make-dlink :payload item :right right))

(defun circular-dlink (&optional (payload nil))
  (let ((cell (make-dlink :payload payload)))
    (setf (dlink-right cell) cell
	  (dlink-left cell) cell)
    cell))
