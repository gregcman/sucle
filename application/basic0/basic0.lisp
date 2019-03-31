(defpackage #:sucle
  (:use #:cl #:utility #:application #:opengl-immediate
	#:sprite-chain #:point #:rectangle)
  (:export #:start))
(in-package :sucle)

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
(defparameter *draw-pic* nil)

(defparameter *with-functions*
  #+nil
  (list
   (lambda (x)
     (print 34)
     (unwind-protect 
	  (funcall x)
       (print 2))))
  (list
   'call-with-zpng-lparallel
   'sandbox::call-with-world-meshing-lparallel))
(defun run-with (fun)
  (flet ((nest (with-fun cont)
	   (lambda ()
	     (funcall with-fun cont))))
    (dolist (with-fun *with-functions*)
      (setf fun (nest with-fun fun))))
  fun)
(defun start ()
  (application:main
   (run-with
    (lambda ()
      (setf (sandbox-sub::entity-fly? testbed::*ent*) nil
	    (sandbox-sub::entity-gravity? testbed::*ent*) t)
      (our-load)
      (let ((text-sub::*text-data-what-type* :framebuffer))
	(unwind-protect
	  (loop
	     (application:poll-app)
	     
	     (testbed::per-frame)
	     (if *app*
		 (progn
		   ;;#+nil
		   (per-frame)
		   #+nil
		   
		   (when (window:skey-j-p (window::keyval #\e))
		     (window::toggle-mouse-capture))))
	     (when *draw-pic*
	       (draw-pic))
	     ;;#+nil
	     (when (window:skey-j-p (window::keyval #\h))
	       (toggle *app*))
	     (when (window:skey-j-p (window::keyval #\u))
	       (toggle *draw-pic*)))
	  (save)))))
   :width (floor (* 80 *glyph-width*))
   :height (floor (* 25 *glyph-height*))
   :title ""))

(defclass sprite ()
  ((bounding-box :accessor sprite.bounding-box
		 :initform (make-instance 'rectangle
					  :x0 -0.25 :y0 -0.25
					  :x1 0.25 :y1 0.25)
		 :initarg :bounding-box)
   (absolute-rectangle :accessor sprite.absolute-rectangle
		       :initform (make-instance 'rectangle)
		       :initarg :absolute-rectangle)
   (string :accessor sprite.string
	   :initform "Hello World"
	   :initarg :string)
   (tickfun :accessor sprite.tickfun
	    :initform nil
	    :initarg :tickfun)
   (onclick :accessor sprite.onclick
	    :initform nil
	    :initarg :onclick)
   (position :accessor sprite.position
	     :initform (make-instance 'point)
	     :initarg :position)))

(defun closest-multiple (x n)
  (* n (round x n)))

(defparameter *mouse-x* 0.0)
(defparameter *mouse-y* 0.0)

(defun random-point ()
  (make-instance 'point
		 :x (* *glyph-width* (random 80))
		 :y (* *glyph-height* (random 25))))

(defun integer-point (x y)
  (make-instance 'point
		 :x (* *glyph-width* x)
		 :y (* *glyph-height* y)))

(defun string-bounding-box (string &optional (rectangle (make-instance 'rectangle)))
  (multiple-value-bind (x y) (string-bounds string)
    (with-slots (x0 y0 x1 y1) rectangle
      (setf x0 0.0
	    y0 (- (* *glyph-height* y))
	    x1 (* *glyph-width* x)
	    y1 *glyph-height*))))
(defun string-bounds (string)
  (let ((len (length string))
	(maxx 0)
	(x 0)
	(y 0))
    (dotimes (index len)
      (let ((char (aref string index)))
	(cond ((char= char #\Newline)
	       (when (> x maxx)
		 (setf maxx x))
	       (setf x 0)
	       (decf y))
	      (t
	       (setf x (1+ x))))))
    (values (max x maxx) y)))

(defparameter *selection* nil)
(defparameter *hovering* nil)
(defparameter *drag-offset-x* 0.0)
(defparameter *drag-offset-y* 0.0)

(defun init ())
(defun app ()
  (setf *mouse-x* (floatify window::*mouse-x*)
	*mouse-y* (- window::*height* (floatify window::*mouse-y*)))
  (when (window::skey-j-p (window::keyval #\esc))
    (pop-sprite-chain-stack))
  (do-sprite-chain (sprite t) ()
    (let ((fun (sprite.tickfun sprite)))
      (when fun
	(funcall fun))))
  (when 
    (window::skey-j-p (window::mouseval 4))
    (typecase *hovering*
      (sprite
       (sprite-chain:remove-sprite *hovering*)
       (setf *hovering* nil))))

  (let ((mousex *mouse-x*)
	(mousey *mouse-y*))
      ;;search for topmost sprite to drag
    (let
	((sprite
	  (block cya
	    (do-sprite-chain (sprite) ()
	      (with-slots (absolute-rectangle) sprite
		(when (coordinate-inside-rectangle-p mousex mousey absolute-rectangle)
		  (return-from cya sprite)))))))
      (setf *hovering* sprite)
      (when sprite	
	(when (window::skey-j-p (window::mouseval :left))
	  (let ((onclick (sprite.onclick sprite)))
	    (when onclick
	      (funcall onclick sprite))))
	(when (window::skey-j-p (window::mouseval 5))
	  (with-slots (position) sprite
	    (with-slots (x y) position
	      (setf *drag-offset-x* (- x mousex)
		    *drag-offset-y* (- y mousey))))
	  (setf *selection* sprite)
	  (topify-sprite sprite))))
    (typecase *selection*
      (sprite (with-slots (x y) (slot-value *selection* 'position)
		(let ((xnew (closest-multiple (+ *drag-offset-x* mousex) *glyph-width*))
		      (ynew (closest-multiple (+ *drag-offset-y* mousey) *glyph-height*)))
		  (unless (eq x xnew)
		    (setf x xnew))
		  (unless (eq y ynew)
		    (setf y ynew)))))))
  (when (window::skey-j-r (window::mouseval 5))
    (setf *selection* nil))
  
  (do-sprite-chain (sprite t) ()
    (update-bounds sprite))
  

  (glhelp:set-render-area 0 0 window:*width* window:*height*)
  (gl:clear-color 0.5 0.25 0.25
		  (byte/255 (text-sub::char-attribute nil nil nil)))
  ;(gl:clear :color-buffer-bit)
  (gl:polygon-mode :front-and-back :fill)
  (gl:disable :cull-face)
  (gl:disable :blend)
  (render-stuff))

(defun update-bounds (sprite)
  (with-slots (bounding-box position absolute-rectangle)
      sprite
    (with-slots (x0 y0 x1 y1) bounding-box
      (with-slots ((xpos x) (ypos y)) position
	(let ((px0 (+ x0 xpos))
	      (py0 (+ y0 ypos))
	      (px1 (+ x1 xpos))
	      (py1 (+ y1 ypos)))
	  (with-slots (x0 y0 x1 y1) absolute-rectangle
	    (setf x0 px0 y0 py0 x1 px1 y1 py1)))))))

(progn
  (deflazy flat-shader-source ()
    (glslgen:ashader
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
  (glhelp:deflazy-gl flat-shader (flat-shader-source)
    (glhelp::create-gl-program flat-shader-source)))

(defun bytecolor (r g b &optional (a 3))
  "each channel is from 0 to 3"
  (byte/255		    
   (text-sub::color-rgba r g b a)
   ))

(defun draw-string
    (x y string &optional
		  (fgcol
		   (bytecolor 0 0 0 3
		    ))
		  (bgcol		   
		   (bytecolor 3 3 3 3)
		   )
		  bold-p
		  underline-p)
  (let ((start x)
	(len (length string))
	(attr (byte/255 (text-sub::char-attribute bold-p underline-p t))))
    (dotimes (index len)
      (let ((char (aref string index)))
	(cond ((char= char #\Newline)
	       (setf x start)
	       (decf y))
	      (t
	       (color (byte/255 (char-code char))
		      bgcol
		      fgcol
		      attr)
	       (vertex (floatify x)
		       (floatify y)
		       0.0)			  
	       (incf x)))))))

(defun render-stuff ()
  (text-sub::with-data-shader (uniform rebase)
    (gl:clear :color-buffer-bit)
    (gl:disable :depth-test)

    ;;"sprites"
    (do-sprite-chain (sprite t) ()
      (with-slots (position string)
	  sprite
	(with-slots ((xpos x) (ypos y)) position
	  (multiple-value-bind (fgcolor bgcolor) 
	    (cond ((eq sprite *selection*)
		   (values
		    (bytecolor 0 3 3 3)
		    (bytecolor 3 0 0 3)
		    ;;(byte/255 1)
		    ;;(byte/255 15)
		    ))
		  ((eq sprite *hovering*)
		   (values
		    (bytecolor 3 3 3)
		    (bytecolor 0 0 0)))
		  (t
		   (values
		    ;;(byte/255 0)
		    (bytecolor 0 0 0)
		    (bytecolor 3 3 3)
		    ;;(byte/255 255)
		    )))
	    (draw-string (/ xpos *glyph-width*)
			 (/ ypos *glyph-height*)
			 string
			 fgcolor
			 bgcolor
			 t
			 nil)))))
    
    (rebase -128.0 -128.0))
  (gl:point-size 1.0)
  (gl:with-primitives :points
    (opengl-immediate::mesh-vertex-color))
;;  (text-sub-test::fuzz)
  (text-sub::with-text-shader (uniform)
    (gl:uniform-matrix-4fv
     (uniform :pmv)
     (load-time-value (nsb-cga:identity-matrix))
     nil)   
    (glhelp::bind-default-framebuffer)
    (glhelp:set-render-area 0 0 (getfnc 'application::w) (getfnc 'application::h))
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (text-sub::draw-fullscreen-quad)))

(defun plain-button (fun &optional
			   (str (string (gensym "nameless-button-")))
			   (pos (random-point))
			   (sprite (make-instance 'sprite)))
  "a statically named button"
  (let ((rect (make-instance 'rectangle)))
    (string-bounding-box str rect)
    (with-slots (position bounding-box string onclick) sprite
      (setf position pos
	    bounding-box rect
	    string str
	    onclick fun)))
  sprite)

(progn
  (defparameter *sprite-chain-stack* nil)
  (defparameter *sprite-chain-stack-depth* 0)
  (defun push-sprite-chain-stack (&optional (new-top (sprite-chain:make-sprite-chain)))
    (push sprite-chain::*sprites* *sprite-chain-stack*)
    (setf sprite-chain::*sprites* new-top)
    (incf *sprite-chain-stack-depth*))
  (defun pop-sprite-chain-stack ()
    (let ((top (pop *sprite-chain-stack*)))
      (when top
	(decf *sprite-chain-stack-depth*)
	(setf sprite-chain::*sprites* top))))
  (defun replace-sprite-chain-stack ()
    (pop-sprite-chain-stack)
    (push-sprite-chain-stack)))

(defun bottom-layer ()
  #+nil
  (add-sprite
   (plain-button
    (lambda (this) (remove-sprite this))
    "hello world"))
  (add-sprite
   (plain-button
    (lambda (this)
      (declare (ignorable this))
      (application::quit))
    "quit"
    (integer-point 0 1)))
  #+nil
  (add-sprite
   (plain-button
    (lambda (this)
      (declare (ignorable this))
      (new-layer))
    "new"))
  
  (let ((rect (make-instance 'rectangle))
	(numbuf (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character)))
    (add-sprite
     (make-instance
      'sprite
      :position (integer-point 10 1)
      :bounding-box rect
      :tickfun
      (lambda ()
	;;mouse coordinates
	(setf (fill-pointer numbuf) 0)
	(with-output-to-string (stream numbuf :element-type 'character)
	  #+nil
	  (princ (list (floor *mouse-x*)
		       (floor *mouse-y*))
		 stream)
	  (princ (aref block-data::*names*
		       testbed::*blockid*)
		 stream)
	  )
	(string-bounding-box numbuf rect))
      :string numbuf
      ))))

(defun new-layer ()
  (push-sprite-chain-stack)
  (add-sprite 
   (plain-button
    (lambda (this)
      (declare (ignorable this))
      (new-layer))
    "new"))
  (add-sprite
   (plain-button
    (lambda (this)
      (declare (ignorable this))
      (pop-sprite-chain-stack))
    "back"))
  (add-sprite
   (plain-button
    nil
    (format nil "layer ~a" *sprite-chain-stack-depth*))))

(progn
  (setf sprite-chain::*sprites* (sprite-chain:make-sprite-chain))
  (bottom-layer))

(defun save ()
  (atest::remove-zeroes)
  (sandbox::msave "test/"))

(defun our-load ()
  (sandbox::mload "test/"))


(setf sandbox::*some-saves*
      (merge-pathnames
       "save/"
       (asdf:system-source-directory :sucle)
		       )
      #+nil
      (cdr (assoc (machine-instance) 
		  '(("gm3-iMac" . #P"/media/imac/share/space/lispysaves/saves/sandbox-saves/")
		    ("nootboke" . #P"/home/terminal256/Documents/saves/"))
		  :test 'equal)))
;;;FIXME::duplicate in terminal625/basic
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
	    (value "vec4")
	    (pmv "mat4"))
      :program
      '(defun "main" void ()
	(= "gl_Position" (* pmv position))
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
       ;(value . 3)
       )
     :varyings
     '((value-out . value)
       (tex-out . tex))
     :uniforms
     '((sampler (:fragment-shader sampler))
       (pmv (:vertex-shader pmv))
       (value (:vertex-shader value)))))
  (glhelp::deflazy-gl flat-texture-shader (flat-texture-shader-source)
    (glhelp::create-gl-program flat-texture-shader-source)))


;;FIXME::see sandbox/change-world.lisp
(defvar *zpng-lparallel-kernel* nil)
(defmacro with-zpng-lparallel-kernel (&body body)
  `(let ((lparallel:*kernel* *zpng-lparallel-kernel*)) ,@body))
(defparameter *zpng-channel* nil)

(defmacro with-zpng-lparallel (&body body)
  `(let ((*zpng-lparallel-kernel* nil))
     (unwind-protect (progn (setf *zpng-lparallel-kernel*
				  (lparallel:make-kernel 2))
			    (with-zpng-lparallel-kernel
			      (let ((*zpng-channel*
				     (lparallel:make-channel)))
				,@body)))
       (when *zpng-lparallel-kernel*
	 (lparallel:end-kernel)))))

(defun call-with-zpng-lparallel (fun)
  (with-zpng-lparallel
    (funcall fun)))

(glhelp::deflazy-gl cons-texture (application::w application::h
				  )
  (setf *finished* nil)
  (make-instance
   'glhelp::gl-texture
   :handle
   (prog1
       (glhelp:pic-texture
	(make-array `(,application::h ,application::w
		      ;;512 512
				      4)) :rgba)
     (glhelp:apply-tex-params
      (quote ((:texture-min-filter . :linear)
	      (:texture-mag-filter . :linear)
	      (:texture-wrap-s . :clamp)
	      (:texture-wrap-t . :clamp)))))))

(defparameter *pic-tint* (vector 1.0 1.0 1.0 1.0))
(defun draw-pic ()
  (gl:polygon-mode :front-and-back :fill)
  (glhelp::bind-default-framebuffer)
  (gl:disable :depth-test)
  (glhelp:set-render-area 0 0
			  ;;512 512
			  (getfnc 'application::w) (getfnc 'application::h)
			  )
  (gl:enable :blend)
  ;(gl:disable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (submit-zpng-draw-task 10 'a-zpng)
  (with-zpng-lparallel-kernel
    (multiple-value-bind (value success-p) (lparallel:try-receive-result *zpng-channel*)
      (if success-p
	  (progn
	    (destructuring-bind (id data) value
	      (remhash id *draw-tasks*)
	      (pushnew id *finished* :test 'eql)
	      (transfer-zpng-data data)))
	  )))
  (let ((program (getfnc 'flat-texture-shader)))
    (glhelp::use-gl-program program)
    (glhelp:with-uniforms uniform program
      (gl:uniformi (uniform 'sampler) 0)
      (glhelp::set-active-texture 0)
      (gl:uniform-matrix-4fv
       (uniform 'pmv)
       (load-time-value (nsb-cga:identity-matrix))
       nil)
      (gl:uniformfv (uniform 'value)
		    *pic-tint*))   
    (gl:bind-texture :texture-2d
		     (glhelp::handle (getfnc 'cons-texture)))
    (text-sub::draw-fullscreen-quad)))


(defparameter *draw-tasks* (make-hash-table :test 'eql))
(defparameter *finished* nil) ;;FIXME:::uses a list, not a hash table
(defun submit-zpng-draw-task (id fun &rest args)
  (unless (or (member id *finished*)
	      (gethash id *draw-tasks*))
    (setf (gethash id *draw-tasks*) fun)
    (with-zpng-lparallel-kernel
      (apply 'lparallel:submit-task
	     *zpng-channel*
	     (lambda (&rest args)
	       (cons
		id
		(multiple-value-list
		 (apply fun args))))
	     args))))

(defun a-zpng ()
  (etouq
    (nth 5
	 '((vecto-test::star-clipping)
	   (vecto-test::feedlike-icon)
	   (vecto-test::gradient-example)
	   (vecto-test::gradient-bilinear-example)
	   (vecto-test::radiant-lambda)
	   (vecto-test::text-paths)
	   (make-instance 'zpng::png
	    :color-type :truecolor-alpha
	    :width 16
	    :height 16)))))

(defun transfer-zpng-data (zpng)
  (let ((texture (getfnc 'cons-texture)))
    (gl:bind-texture :texture-2d (glhelp::handle texture))
    (gl:tex-sub-image-2d :texture-2d 0 0 0
			 (zpng:width zpng)
			 (zpng:height zpng)
			 :rgba :unsigned-byte (flip-zpng-image
					       (zpng:height zpng)
					       (zpng:image-data zpng)))))

(defun flip-zpng-image (height image)
  (let ((longjumps (/ (length image) height)))
    (declare (type fixnum height longjumps))
    (let ((magic (* longjumps (- height 1))))
      (loop for h below (* longjumps (- height (floor height 2))) by longjumps do
	   (loop for w below longjumps do
		(rotatef (row-major-aref image (+ (- magic h) w))
			 (row-major-aref image (+ h w)))))))
  image)
