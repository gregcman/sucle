(in-package :sandbox) 

(defstruct vao
  id
  length
  verts
  indices)

(defun squish (the-list type &key (biglength (length the-list)))
  "turns a list of identical vertices/indicis into a flat array for opengl"
  (let* ((siz (length (car the-list)))
	 (verts (make-array (* siz biglength) :element-type type))
	 (counter 0))
    (dolist (vert the-list)
      (dotimes (item siz)
	(setf (aref verts counter) (elt vert item))
	(incf counter)))
    verts))

(defun shape-vao (s)
  "converts a shape into a vao"
  (create-vao
   (shape-vs s)
   (shape-is s)))

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

    (let ((totsize (* 17 (sizeof :float))))
      ;;position
      (gl:vertex-attrib-pointer
       0 3 :float :false totsize 0)
      (gl:enable-vertex-attrib-array 0)

      ;;texcoord
      (gl:vertex-attrib-pointer
       2 2 :float :false totsize (* 3 (sizeof :float)))
      (gl:enable-vertex-attrib-array 2)

      ;;color
      (gl:vertex-attrib-pointer
       3 4 :float :false totsize (* 5 (sizeof :float)))
      (gl:enable-vertex-attrib-array 3)

      ;;blocklight
      (gl:vertex-attrib-pointer
       8 4 :float :false totsize (* 9 (sizeof :float)))
      (gl:enable-vertex-attrib-array 8)

      ;;skylight
      (gl:vertex-attrib-pointer
       12 4 :float :false totsize (* 13 (sizeof :float)))
      (gl:enable-vertex-attrib-array 12))
    
    (gl:free-gl-array glverts)
    (gl:free-gl-array glindices)

    (gl:bind-vertex-array 0)
    (make-vao
     :id vertex-array-object
     :length (length indices)
     :verts glverts
     :indices glindices)))

(let ((nullarray (gl:make-null-gl-array :unsigned-int)))
  (defun draw-vao (some-vao)
    "draws a vao struct"
    (gl:bind-vertex-array (vao-id some-vao))
    (if nil
	(gl:draw-arrays
	 :quads
	 0
	 (vao-length some-vao))
	(gl:draw-elements 
	 :triangles
	 nullarray
	 :count (vao-length some-vao)))))
