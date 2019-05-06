(defpackage #:vecto-stuff
  (:use :cl :utility :application))
(in-package :vecto-stuff)

;;;FIXME::duplicate in terminal625/basic

(glhelp::deflazy-gl flat-texture-shader ()
  (glhelp::create-opengl-shader
   "
out vec4 value_out;
out vec2 tex_out;
in vec4 position;
in vec2 tex;
uniform vec4 value;
uniform mat4 pmv;

void main () {
gl_Position = pmv * position;
value_out = value;
tex_out = tex;
}"
   "
in vec4 value_out;
in vec2 tex_out;
uniform sampler2D sampler;
void main () {
vec4 pixdata = texture2D(sampler,tex_out);
gl_FragColor = pixdata * value_out;
}"
   '(("position" 0)
     ("tex" 2)
     ;;("value" . 3)
     )
   '((sampler "sampler")
     (pmv "pmv")
     (value "value"))))


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

(glhelp::deflazy-gl cons-texture (;;application::w application::h
				  )
  (setf *finished* nil)
  (let ((texture
	 (glhelp::create-opengl-texture-from-data
	  (image-utility::load-image-from-file
	   ;;FIXME::allow flipping of images
	   "/home/imac/Documents/handwritten\ notes/batch\ 1/IMG_20190321_012106.615.png"
	   :flip t))))
    (gl:bind-texture :texture-2d texture)
    (glhelp:apply-tex-params
      (quote ((:texture-min-filter . :linear)
	      (:texture-mag-filter . :linear)
	      (:texture-wrap-s . :clamp)
	      (:texture-wrap-t . :clamp))))
    (glhelp::wrap-opengl-texture texture)))

(defparameter *pic-tint* (vector 1.0 1.0 1.0 1.0))
(defparameter *ticks* 0)
(defun draw-pic ()
  (incf *ticks*)
  #+nil
  (progn
    (submit-zpng-draw-task 10 'a-zpng)
    (when (zerop (mod *ticks* 4))
      (setf *finished* nil))
    (with-zpng-lparallel-kernel
      (multiple-value-bind (value success-p) (lparallel:try-receive-result *zpng-channel*)
	(if success-p
	    (progn
	      (destructuring-bind (id data) value
		(remhash id *draw-tasks*)
		(pushnew id *finished* :test 'eql)
		(transfer-zpng-data data)))
	    ))))

  (draw-texture
   (glhelp::handle (getfnc 'cons-texture))))

(defun draw-texture (texture)
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

  (let ((program (getfnc 'flat-texture-shader)))
    (glhelp::use-gl-program program)
    (glhelp:with-uniforms uniform program
      (glhelp::set-uniforms-to-textures
       ((uniform 'sampler) texture))
      (gl:uniform-matrix-4fv
       (uniform 'pmv)
       (load-time-value (nsb-cga:identity-matrix))
       nil)
      (gl:uniformfv (uniform 'value)
		    *pic-tint*))   
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
    (nth 4
	 '((vecto-test::star-clipping)
	   (vecto-test::feedlike-icon)
	   (vecto-test::gradient-example)
	   (vecto-test::gradient-bilinear-example)
	   (vecto-test::graph)
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
