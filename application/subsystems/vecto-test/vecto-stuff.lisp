(defpackage #:vecto-stuff
  (:use :cl :utility :application))
(in-package :vecto-stuff)

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
 
