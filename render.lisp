(in-package :sandbox) 

(defun caption-info ()
  (window:set-caption
   (concatenate 'string
		in::pressed-keys '(#\:)
		in::down-keys '(#\:)
		in::released-keys)))

(defun draw ()
  (caption-info)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (render)
  (gl:flush)
  (sdl:update-display))

(defun sizeof (type-keyword)
  (cffi:foreign-type-size type-keyword))
(defun rad-deg (rad)
  (* rad 180 (/ 1 pi)))
(defun deg-rad (deg)
  (* deg pi 1/180))

(defparameter cameraPos (mat:onebyfour '(0.0 0.0 0.0 1)))
(defparameter cameraVelocity (mat:onebyfour '(0.0 0.0 0.0 0)))
(defparameter up (mat:onebyfour '(0.0 1.0 0.0 0)))
(defparameter yaw 0)
(defparameter pitch 0)

(defparameter shaderProgram nil)

(defparameter fov 100)

(defun set-matrix (name matrix)
  (gl:uniform-matrix-4fv
   (gl:get-uniform-location shaderProgram name)
   (mat:to-flat matrix)))

(defun set-int (name thenumber)
  (gl:uniformi
   (gl:get-uniform-location shaderProgram name)
   thenumber))

(defun load-and-make-shader (vpath fpath)
  (make-shader-program-from-strings
   (load-shader-file vpath)
   (load-shader-file fpath)))

(defun glinnit ()
  (if nil
      (gl:clear-color 0 0 0 0)
      (gl:clear-color 0.68 0.8 1.0 1.0))
  (gl:enable :depth-test)
  (gl:disable :blend)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:active-texture :texture0)
  
  (setq shaderProgram
	(load-and-make-shader
	 "transforms.vs" "basictexcoord.frag"))
  (gl:use-program shaderProgram))

(defun glActiveTexture (num)
  (gl:active-texture (+ num (get-gl-constant :texture0))))

(defun get-gl-constant (keyword)
  (cffi:foreign-enum-value '%gl:enum keyword))

(defstruct vao
  id
  length
  verts
  indices)

(defun squish (the-list type &key (biglength (length the-list)))
  (let* ((verts (make-array (* (length (car the-list)) biglength) :element-type type))
	 (counter 0))
    (dolist (vert the-list)
      (dolist (item vert)
	(setf (aref verts counter) item)
	(incf counter)))
    verts))

(defun shape-vao (s)
  (let ((verts (squish (reverse (shape-vs s)) :float :biglength (shape-vertlength s)))
	(indicies (squish (shape-is s) :unsigned-int)))
    (create-vao
     verts
     indicies)))

(defun to-gl-array (seq type
                    &key
                      (length (length seq))
                      (array (gl:alloc-gl-array type length)))
  (declare (optimize speed))
  (let ((pointer (gl::gl-array-pointer array)))
    (dotimes (i length)
      (setf (cffi:mem-aref pointer type i) (row-major-aref seq i))))
  array)

(defun destroy-vao (vao)
  (gl:delete-vertex-arrays (list (vao-id vao))))

(defun create-vao (vertices indices)
  (let ((vertex-array-object (gl:gen-vertex-array))
	(glverts (to-gl-array vertices :float))
	(glindices (to-gl-array indices :unsigned-int)))
    (gl:bind-vertex-array vertex-array-object)
     
    (gl:bind-buffer :array-buffer (gl:gen-buffer))
    (gl:buffer-data :array-buffer :static-draw
		    glverts)
    (gl:bind-buffer :element-array-buffer (gl:gen-vertex-array))
    (gl:buffer-data :element-array-buffer :static-draw
		    glindices) 
     ;;position attribute


    (gl:vertex-attrib-pointer
     0 3 :float :false (* 9 (sizeof :float)) 0)
    (gl:enable-vertex-attrib-array 0)

     ;;texture attribute


    (gl:vertex-attrib-pointer
     2 2 :float :false (* 9 (sizeof :float)) (* 3 (sizeof :float)))
    (gl:enable-vertex-attrib-array 2)

    (gl:vertex-attrib-pointer
     4 4 :float :false (* 9 (sizeof :float)) (* 5 (sizeof :float)))
    (gl:free-gl-array glverts)
    (gl:free-gl-array glindices)

    (gl:bind-vertex-array 0)
    (make-vao
     :id vertex-array-object
     :length (length indices)
     :verts glverts
     :indices glindices)))

(defun draw-vao (some-vao)
  (gl:bind-vertex-array (vao-id some-vao))
  (gl:draw-elements
   :triangles
   (gl:make-null-gl-array :unsigned-int)
   :count (vao-length some-vao))
  (gl:bind-vertex-array 0))

(defun create-texture-wot (tex-data width height)
  (let ((the-shit (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d the-shit)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-border-color '(0 0 0 0))
    (gl:tex-image-2d
     :texture-2d 0
     :rgba width height 0 :rgba :unsigned-byte tex-data)
    (gl:generate-mipmap :texture-2d)
    the-shit))

(defun bind-shit (name)
  (let ((num (gethash name texture-library)))
    (gl:bind-texture :texture-2d num)))

(defun make-shader-program-from-strings
    (vertex-shader-string fragment-shader-string)
  (let ((vertexShader (gl:create-shader :vertex-shader))
	(fragmentShader (gl:create-shader :fragment-shader))
	(shaderProgram (gl:create-program)))
    (gl:shader-source vertexShader vertex-shader-string)
    (gl:compile-shader vertexShader)
    (let ((success (gl:get-shader-info-log vertexShader)))
      (unless (zerop (length success))
	(print success)))
    (gl:shader-source fragmentShader fragment-shader-string)
    (gl:compile-shader fragmentShader)
    (let ((success (gl:get-shader-info-log fragmentShader)))
      (unless (zerop (length success))
	(print success)))
    (gl:attach-shader shaderProgram vertexShader)
    (gl:attach-shader shaderProgram fragmentShader)
    (gl:link-program shaderProgram)
    (let ((success (gl:get-program-info-log shaderProgram)))
      (unless (zerop (length success))
	(print success)))
    (gl:delete-shader vertexShader)
    (gl:delete-shader fragmentShader)
    shaderProgram))
