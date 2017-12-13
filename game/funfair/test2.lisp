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
(deflazy font-texture (:opengl)
  (prog1
      (glhelp:pic-texture
       (getfnc 'font-png)
       :luminance)
    (glhelp:apply-tex-params
     (quote ((:texture-min-filter . :nearest)
	     (:texture-mag-filter . :nearest)
	     (:texture-wrap-s . :repeat)
	     (:texture-wrap-t . :repeat))))))

(defparameter *identity-mat*
  (cg-matrix:identity-matrix))
(defparameter *view* (make-instance 'funfair::render-area))
(defun per-frame (session)
  (declare (ignorable session))
  (unless (eq (glhelp::gl-program-object-src (getfnc 'text-shader))
	      *shader-test*)
    (gl:delete-program (glhelp::gl-program-object-handle
			(getfnc 'text-shader)))
    (funfair::remove-stuff 'text-shader))
  (setf (render-area-width *view*) window::*width*
	(render-area-height *view*) window::*height*)
  (set-render-area *view*)
  (let ((program (getfnc 'text-shader)))
    (glhelp::use-gl-program program)
    (glhelp:with-uniforms uniform program
      (gl:uniform-matrix-4fv
       (uniform :pmv)
       *identity-mat*
       nil))
    (gl:disable :cull-face)
    (gl:bind-texture :texture-2d (getfnc 'font-texture))
    (gl:call-list (getfnc 'huh?))))

(defun use-text ()
  (setf *trampoline* 'per-frame)
  (setf *pre-trampoline-hooks* nil))

(defmacro progeach (fun body)
  `(etouq
    (cons 'progn
	  (mapcar ,fun
		  ,body))))

(deflazy huh? (:opengl)
 (let ((a (scratch-buffer:my-iterator))
       (c (scratch-buffer:my-iterator))
       (b (scratch-buffer:my-iterator))
       (len 0))
   (iter-ator:bind-iterator-out
    (col single-float) c
    (iter-ator:bind-iterator-out
     (pos single-float) a
     (iter-ator:bind-iterator-out
      (tex single-float) b

      (progeach
       (lambda (x) (list 'pos x))
       (axis-aligned-quads:quadk+ 0.0 '(-1.0 1.0 -1.0 1.0)))
      (progeach
       (lambda (x) (list 'tex x))
       (axis-aligned-quads:duaq 1 nil '(0.0 1.0 0.0 1.0)))
      
      (progeach
       (lambda (x) (list 'col x))
       '(1.0 0.0 0.0 0.0
	 0.0 0.0 0.0 0.0
	 0.0 1.0 0.0 0.0
	 1.0 1.0 0.0 0.0)))
     (incf len 4)
     ))
   (glhelp:with-gl-list
     (gl:with-primitives :quads
       (scratch-buffer:flush-my-iterator a
	 (scratch-buffer:flush-my-iterator c
	   (scratch-buffer:flush-my-iterator b
	     ((lambda (times a c b)
		(iter-ator:bind-iterator-in
		 (xyz single-float) a
		 (iter-ator:bind-iterator-in
		  (dark single-float) c
		  (iter-ator:bind-iterator-in
		   (tex single-float) b
		   (dotimes (x times)
		     (%gl:vertex-attrib-2f 8 (tex) (tex))
		     (%gl:vertex-attrib-4f 2 (xyz) (xyz) (xyz) 1.0)
		     (%gl:vertex-attrib-4f 0 (dark) (dark) (dark) (dark)))))))
	      len a c b))))))))

(defparameter *shader-test*
  (let (a)
    (setf
     a
     (make-instance
      'glslgen:shader-program-data
      :version 330
      :vs
      (glslgen2::make-shader-stage
       :out '((color-out "vec3")
	      (texcoord-out "vec2"))
       :in '((position "vec4")
	     (texcoord "vec2")
	     (color "float")
	     (projection-model-view "mat4"))
       :program
       '(defun main void ()
	 (= gl-position (* projection-model-view position))
	 (= color-out (vec3 color))
	 (= texcoord-out texcoord)))
      :frag
      (glslgen2::make-shader-stage
       :in '((texcoord "vec2")
	     (color "vec3")
	     (sampler "sampler2D"))
       :program
       '(defun main void ()
	 (/**/ vec4 pixdata)
	 (= pixdata (texture2d sampler texcoord))
	 (= (|.| :gl-frag-color rgb)
	  ;(* (/ (|.| gl-frag-coord xyz) 1000.0))
	  (* color (|.| pixdata rgb))
	  )))
      :attributes
      '((position . 2) 
	(texcoord . 8)
	(color . 0))
      :varyings
      '((color-out . color)
	(texcoord-out . texcoord))
      :uniforms
      '((:pmv (:vertex-shader projection-model-view)))))
    (glslgen:dump-shader-program-data a)
    a))

(deflazy text-shader (:opengl)
  (glhelp::create-gl-program *shader-test*))

