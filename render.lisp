(in-package :sandbox)

;;this file is the place where all the generic rendering shit goes.
;;creating vaos, loading shaders

(defun sizeof (type-keyword)
  "gets the size of a foreign c type"
  (cffi:foreign-type-size type-keyword))

(defun rad-deg (rad)
  "converts radians to degrees"
  (* rad 180 (/ 1 pi)))
(defun deg-rad (deg)
  "converts degrees to radians"
  (* deg pi 1/180))

(defstruct simplecam
  (pos (mat:onebyfour '(0.0 0.0 0.0 1)))
  (up (mat:onebyfour '(0.0 1.0 0.0 0)))
  (yaw 0)
  (pitch 0)
  (fov 100))

(defparameter shaderProgram nil)

(defun set-matrix (name matrix)
  "sets a uniform matrix"
  (gl:uniform-matrix-4fv
   (gl:get-uniform-location shaderProgram name)
   (mat:to-flat matrix)))

(defun set-int (name thenumber)
  "sets a uniform integer"
  (gl:uniformi
   (gl:get-uniform-location shaderProgram name)
   thenumber))

(defun load-and-make-shader (vpath fpath)
  "loads a shader from a filepath and puts it into a program"
  (make-shader-program-from-strings
   (load-shader-file vpath)
   (load-shader-file fpath)))

(defun glActiveTexture (num)
  "sets the active texture"
  (gl:active-texture (+ num (get-gl-constant :texture0))))

(defun get-gl-constant (keyword)
  "gets a gl-constant"
  (cffi:foreign-enum-value '%gl:enum keyword))

(defstruct vao
  id
  length
  verts
  indices)

(defun squish (the-list type &key (biglength (length the-list)))
  "turns a list of identical vertices/indicis into a flat array for opengl"
  (let* ((verts (make-array (* (length (car the-list)) biglength) :element-type type))
	 (counter 0))
    (dolist (vert the-list)
      (dolist (item vert)
	(setf (aref verts counter) item)
	(incf counter)))
    verts))

(defun shape-vao (s)
  "converts a shape into a vao"
  (let ((verts (squish (reverse (shape-vs s)) :float :biglength (shape-vertlength s)))
	(indicies (squish (shape-is s) :unsigned-int)))
    (create-vao
     verts
     indicies)))

(defun to-gl-array (seq type
                    &key
                      (length (length seq))
                      (array (gl:alloc-gl-array type length)))
  "writes an array for opengl usage"
  (declare (optimize speed))
  (time
   (let ((pointer (gl::gl-array-pointer array)))
     (print length)
     (dotimes (i length)
       (setf (cffi:mem-aref pointer type i) (row-major-aref seq i)))))
  array)

(defun to-gl-array-uint (seq
			 &key
			 (length (length seq))
			 (array (gl:alloc-gl-array :unsigned-int length)))
  "writes an array for opengl usage"
  (declare (optimize speed))
  (let ((pointer (gl::gl-array-pointer array)))
    (dotimes (i length)
      (setf (cffi:mem-aref pointer :unsigned-int i) (row-major-aref seq i))))
  array)

(defun to-gl-array-float (seq
			  &key
			  (length (length seq))
			  (array (gl:alloc-gl-array :float length)))
  "writes an array for opengl usage"
  (declare (optimize speed))
  (let ((pointer (gl::gl-array-pointer array)))
    (dotimes (i length)
      (setf (cffi:mem-aref pointer :float i) (row-major-aref seq i))))
  array)

(defun destroy-vao (vao)
  "currently unused function to destroy vaos which are done"
  (gl:delete-vertex-arrays (list (vao-id vao))))

(defun create-vao (vertices indices)
  "creates a vao from a list of vertices and indices"
  (let ((vertex-array-object (gl:gen-vertex-array))
	(glverts (to-gl-array-float vertices))
	(glindices (to-gl-array-uint indices)))
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
     (gl:enable-vertex-attrib-array 4)
    
    (gl:free-gl-array glverts)
    (gl:free-gl-array glindices)

    (gl:bind-vertex-array 0)
    (make-vao
     :id vertex-array-object
     :length (length indices)
     :verts glverts
     :indices glindices)))

(defun draw-vao (some-vao)
  "draws a vao struct"
  (gl:bind-vertex-array (vao-id some-vao))
  (gl:draw-elements
   :triangles
   (gl:make-null-gl-array :unsigned-int)
   :count (vao-length some-vao))
  (gl:bind-vertex-array 0))

(defun create-texture-wot (tex-data width height)
  "creates an opengl texture from data"
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
  "bind a texture located in the texture library"
  (let ((num (gethash name texture-library)))
    (gl:bind-texture :texture-2d num)))

(defun make-shader-program-from-strings
    (vertex-shader-string fragment-shader-string)
     "makes a shader program from strings. makes noises if something goes wrong"
  (let ((vertexShader (gl:create-shader :vertex-shader))
	(fragmentShader (gl:create-shader :fragment-shader))
	(shaderProgram (gl:create-program)))
    (gl:bind-attrib-location shaderprogram 0 "position")
    (gl:bind-attrib-location shaderprogram 2 "texCoord")
    (gl:bind-attrib-location shaderprogram 4 "color")
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
