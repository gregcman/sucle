(defpackage #:broad
  (:use :cl :application))
(in-package #:broad)

(defparameter *path*
  "/home/imac/Documents/stuff2/NightFox/NightFox.obj")
(defparameter *path-texture* "/home/imac/Documents/stuff2/NightFox/nightfox_d_4.png")

(deflazy the-mesh ()
  (multiple-value-bind (va ia layout type) (wavefront-obj-to-vertex-and-index-buffer *path*)
    (list va ia layout type)))

(glhelp::deflazy-gl lady-vertex-array (the-mesh)
  (destructuring-bind (vertbuf indexbuf layout type) the-mesh
    (glhelp::make-vertex-array vertbuf indexbuf layout type)))

(deflazy :lady-png ()
  (image-utility:read-png-file
   *path-texture*
   t))
(glhelp:deflazy-gl :lady ()
  (let ((texture (glhelp::pic-texture
		  (getfnc :lady-png)
		  :rgb)))
    (prog1 (make-instance 'glhelp::gl-texture
			  :handle texture)
      (glhelp::apply-tex-params
       (quote ((:texture-min-filter . :linear)
	       (:texture-mag-filter . :linear)
	       (:texture-wrap-s . :repeat)
	       (:texture-wrap-t . :repeat)))))))

(defun draw-baggins ()
  ;;  (gl:disable :cull-face :blend)
  ;;    (gl:polygon-mode :front-and-back :fill)
  (glhelp::draw-vertex-array (getfnc 'lady-vertex-array)))

(defun wavefront-obj-to-vertex-and-index-buffer
    (&optional (path *path*))
  "parse a wavefront obj into a vertex array and index array for transfer to opengl"
  (let ((woywoy (cl-mesh:parse-wavefront-obj path))
	(unique-vertices (make-hash-table :test 'equalp)))
    (generate-vertex-hash unique-vertices (gethash "indices" woywoy))
    (values
     ;;vertex array buffer
     (let ((vertarray (order-vertices unique-vertices)))
       (flatten-vert vertarray (gethash "vertices" woywoy)
		     (gethash "uv" woywoy)))
     ;;index array buffer
     (gen-index-buf unique-vertices (gethash "indices" woywoy))
     ;; In this program, we use attribute 0 for position. If you had
     ;; per-vertex normals, you could use a different attribute for those
     ;; as well.
     (glhelp::simple-vertex-array-layout
      '((0 4)
	(3 2)))
     :triangles)))
(defun generate-vertex-hash (unique-vertices indexes)
  (let* ((name 0)
	 (len (array-total-size indexes))
	 (hash unique-vertices))
    (flet ((ass (vec)
	     (let ((value (gethash vec hash)))
	       (if value
		   nil
		   (progn (setf (gethash vec hash) name)
			  (incf name))))))
      (dotimes (i len)
	(let ((vec (aref indexes i)))
	  (let ((a (aref vec 0))
		(b (aref vec 1))
		(c (aref vec 2)))

	    (ass a)
	    (ass b)
	    (ass c)))))))
(defun order-vertices (unique-vertices)
  (let ((hash unique-vertices))
    (let ((array (make-array (hash-table-count hash))))
      (maphash (lambda (k v)
		 (setf (aref array v) k))
	       hash)
      array)))
(defun flatten-vert (vertarray verts uv)
  (let* ((array vertarray)
	 (len (array-total-size array))
	 (stride 6))
    (let ((buf (make-array (* len stride))))
      (dotimes (index len)
	(let ((vec (aref array index)))
	  (let ((base (* index stride)))
	    (let ((verts (aref verts (aref vec 0)))
		  (uv (aref uv (aref vec 1))))
	      (setf (aref buf (+ base 0)) (aref verts 0))
	      (setf (aref buf (+ base 1)) (aref verts 1))
	      (setf (aref buf (+ base 2)) (aref verts 2))
	      (setf (aref buf (+ base 3)) 1.0)
	      (setf (aref buf (+ base 4)) (aref uv 0))
	      (setf (aref buf (+ base 5)) (aref uv 1))
	      ))))
      buf)))
(defun gen-index-buf (unique-vertices indexes)
  (let* ((hash unique-vertices)
	 (len (array-total-size indexes))
	 (arr (make-array (* 3 len))))
    (dotimes (i len)
      (let ((vec (aref indexes i))
	    (base (* 3 i)))
	(let ((a (aref vec 0))
	      (b (aref vec 1))
	      (c (aref vec 2)))
	  (setf (aref arr base) (gethash a hash))
	  (setf (aref arr (+ 1 base)) (gethash b hash))
	  (setf (aref arr (+ 2 base)) (gethash c hash)))))
    arr))

(glhelp:deflazy-gl nightfox (nightfox-text)
  (glhelp::create-gl-program nightfox-text))
(application:deflazy nightfox-text ()
  (glslgen::ashader
   :vs
   (glslgen2::make-shader-stage
    :out '((tex-out "vec2"))
    :in '((position "vec4")
	  (tex "vec2")
	  (projection-model-view "mat4"))
    :program
    '(defun "main" void ()
      (= "gl_Position" (* projection-model-view position))
      (= tex-out tex)))
   :frag
   (glslgen2::make-shader-stage
    :in '((tex "vec2")
	  (texture "sampler2D"))
    :program
    '(defun "main" void ()
      (= :gl-frag-color
       ("texture2D" texture tex))))
   :attributes
   '((position . 0) 
     (tex . 3))
   :varyings
   '((tex-out . tex))
   :uniforms
   '((:pmv (:vertex-shader projection-model-view))
     (:sampler (:fragment-shader texture)))))

(defun start ()
  (application::main
   (lambda ()
     (sandbox::with-world-meshing-lparallel
       (loop
	  (application:poll-app)
	  ;;#+nil
	  (testbed::per-frame)
	  (progn
	    (glhelp::set-render-area 0 0 window::*width* window::*height*)
	    (let ((shader (application:getfnc 'broad::nightfox)))
	      (glhelp::use-gl-program shader)
	      ;;uniform crucial for first person 3d
	      (glhelp:with-uniforms
	       uniform shader
	       (gl:uniform-matrix-4fv 
		(uniform :pmv)
		(camera-matrix:camera-matrix-projection-view-player sandbox-sub::*camera*)
		nil)
	       (progn
		 (gl:uniformi (uniform :sampler) 0)
		 (glhelp::set-active-texture 0)
		 (gl:bind-texture :texture-2d 
				  (glhelp::handle (application::getfnc :lady)))))
	      (gl:polygon-mode :front-and-back :fill)
	      (gl:enable :cull-face)
	      (broad::draw-baggins)))
	  )))
   :width 720
   :height 480
   :title "conceptually simple block game"))
