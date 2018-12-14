(defpackage #:broad
  (:use :cl :application))
(in-package #:broad)

(glhelp::deflazy-gl lady-vertex-array ()
  (let ((value (glhelp::make-vertex-array)))
    (fill-vertex-array-object
     (glhelp::vertex-array value)
     (glhelp::vertex-buffer value)
     (glhelp::index-buffer value))
    value))

(deflazy :lady-png ()
  (image-utility:flip-image 
   (image-utility:read-png-file
    "/home/imac/Documents/stuff2/NightFox/nightfox_d_4.png")))
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
  (let ((w (getfnc 'lady-vertex-array)))
  ;;  (gl:disable :cull-face :blend)
;;    (gl:polygon-mode :front-and-back :fill)
    (gl:bind-vertex-array (glhelp::vertex-array w))
    
    ;; This call actually does the rendering. The vertex data comes from
    ;; the currently-bound VAO. If the input array is null, the indices
    ;; will be taken from the element array buffer bound in the current
    ;; VAO.
    (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int)
		      :count (* 3 (array-total-size (gethash "indices" woywoy)))
		      :offset 0)))

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
(defparameter woywoy
  (cl-mesh:parse-wavefront-obj "/home/imac/Documents/stuff2/NightFox/NightFox.obj"))
(defparameter unique-vertices (make-hash-table :test 'equalp))
(defparameter vertarray nil)
(defparameter vertbuf nil)
(progn (generate-vertex-hash unique-vertices (gethash "indices" woywoy))
       (setf vertarray (order-vertices unique-vertices))
       (setf vertbuf (flatten-vert vertarray (gethash "vertices" woywoy)
				   (gethash "uv" woywoy))))

(defun fill-vertex-array-object (vertex-array vertex-buffer index-buffer)  
  (gl:bind-buffer :array-buffer vertex-buffer)
  (let ((verts vertbuf))
    (let ((arr (gl:alloc-gl-array :float (length verts))))
      (dotimes (i (array-total-size verts))
	(setf (gl:glaref arr i) (aref verts i)))
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr)))
  ;; 0 is always reserved as an unbound object.
  (gl:bind-buffer :array-buffer 0)

  ;; An element array buffer stores vertex indices. We fill it in the
  ;; same way as an array buffer.
  (gl:bind-buffer :element-array-buffer index-buffer)
  (let ((hash unique-vertices))
    (let* ((indexes (gethash "indices" woywoy))
	   (len (array-total-size indexes)))
      (let ((arr (gl:alloc-gl-array :unsigned-int (* 3 len))))
	(dotimes (i len)
	  (let ((vec (aref indexes i))
		(base (* 3 i)))
	    (let ((a (aref vec 0))
		  (b (aref vec 1))
		  (c (aref vec 2)))
	      (setf (gl:glaref arr base) (gethash a hash))
	      (setf (gl:glaref arr (+ 1 base)) (gethash b hash))
	      (setf (gl:glaref arr (+ 2 base)) (gethash c hash)))))
	
	(gl:buffer-data :element-array-buffer :static-draw arr)
	(gl:free-gl-array arr))))
  (gl:bind-buffer :element-array-buffer 0)

  ;; Vertex array objects manage which vertex attributes are
  ;; associated with which data buffers. 
  (gl:bind-vertex-array vertex-array)

  ;; To associate our VBO data with this VAO, we bind it, specify
  ;; which vertex attribute we want to associate it with, and specify
  ;; where the data comes from.
  (gl:bind-buffer :array-buffer vertex-buffer)
  ;; In this program, we use attribute 0 for position. If you had
  ;; per-vertex normals, you could use a different attribute for those
  ;; as well.
  (gl:enable-vertex-attrib-array 0)
  ;; Using a null pointer as the data source indicates that we want
  ;; the vertex data to come from the currently bound array-buffer.
  (gl:vertex-attrib-pointer 0 4 :float nil (* 4 6) 0)

  (gl:enable-vertex-attrib-array 3)
  (gl:vertex-attrib-pointer 3 2 :float nil (* 4 6) (* 4 4))



  ;; To associate an element array with this VAO, all we need to do is
  ;; bind the element array buffer we want to use.
  (gl:bind-buffer :element-array-buffer index-buffer)

  ;; Once we're done, we can unbind the VAO, and rebind it when we want to render it.
  (gl:bind-vertex-array 0))

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
