(in-package :glhelp)

;;;;holds a token representing a valid gl context. objects which are not current
;;;;have old tokens.
(defparameter *gl-context* nil)

(defclass gl-object ()
  ((handle :accessor handle
	   :initarg :handle)
   (context :accessor context
	    :initform *gl-context*)))
(defgeneric gl-delete* (obj))
(defmethod gl-delete* :after ((obj gl-object))
  (slot-makunbound obj 'handle))

(defun alive-p (obj)
  (eq *gl-context*
      (context obj)))
(export '(alive-p))

;;;;framebuffers
(defclass gl-framebuffer (gl-object)
  ((texture :accessor texture)
   (depth :accessor depth)
   (x :accessor x)
   (y :accessor y)))

(defun make-gl-framebuffer (width height)
  (let ((inst (make-instance 'gl-framebuffer)))
    (with-slots (x y handle texture depth) inst
      (setf x width
	    y height)
      (setf (values texture handle depth)
	    (glhelp::create-framebuffer width height)))
    inst))

(defmethod gl-delete* ((obj gl-framebuffer))
  (destroy-gl-framebuffer obj))
(defun destroy-gl-framebuffer (gl-framebuffer)
  (gl:delete-renderbuffers (list (depth gl-framebuffer)))
  (gl:delete-framebuffers (list (handle gl-framebuffer)))
  (gl:delete-textures (list (texture gl-framebuffer))))


;;;;program objects
(defun cache-program-uniforms (program uniforms)
  (mapcar
   (lambda (args)
     (destructuring-bind (id . string) args
       (cons
	id
	(gl:get-uniform-location program string))))
   uniforms))
(defun getuniform (shader-info name)
  (cdr (assoc name shader-info :test 'eq)))
(defmacro with-uniforms (name program-object &body body)
  (let ((uniforms-var (gensym)))
    `(let ((,uniforms-var (gl-program-object-uniforms ,program-object)))
       (macrolet ((,name (id)
		    (list 'getuniform ',uniforms-var id)))
	 ,@body))))

(export '(getuniform cache-program-uniforms make-uniform-cache with-uniforms))
(defclass gl-program (gl-object)
  ((src :accessor gl-program-object-src
	:initarg :src)
   (uniforms :accessor gl-program-object-uniforms)))

(defun use-gl-program (src)
  (gl:use-program (handle src)))

(defmethod gl-delete* ((obj gl-program))
  (gl:delete-program (handle obj)))

;;;;textures

(defclass gl-texture (gl-object)
  ())
(defmethod gl-delete* ((obj gl-texture))
  (gl:delete-texture (handle obj)))

;;;;call lists

(defclass gl-list (gl-object)
  ())
(defmethod gl-delete* ((obj gl-list))
  (gl:delete-lists (handle obj) 1))

;;;;vertex array objects

(defclass vao (gl-object)
  ((vbuff :accessor vertex-buffer)
   (ibuff :accessor index-buffer)

   ;;control whether the vertex buffer or index buffer are cleaned up
   ;;upon removal of the vao.
   (v-delete-p :accessor v-delete-p :initform t)
   (i-delete-p :accessor i-delete-p :initform t)
   
   (va :accessor vertex-array)
   (indices :accessor indices :initform 0)
   (render-type :accessor render-type :initform :triangles)))
(defun delete-vao (vao)
  (gl:delete-vertex-arrays (list (vertex-array vao)))

  ;;share the index buffer because it is repetitive in the case of converting from quads
  ;;to triangles
  (when (v-delete-p vao)
    (gl:delete-buffers (list (vertex-buffer vao))))
  (when (i-delete-p vao)
    (gl:delete-buffers (list (index-buffer vao)))))
(defmethod gl-delete* ((obj vao))
  (delete-vao obj))

(defmacro bind-to-array-buffer ((vertex-buffer) &body body)
  `(progn
     (gl:bind-buffer :array-buffer ,vertex-buffer)
     (multiple-value-prog1
	 (locally ,@body)
       ;; 0 is always reserved as an unbound object.
       (gl:bind-buffer :array-buffer 0))))
(defmacro bind-to-element-array-buffer ((index-buffer) &body body)
  `(progn
     (gl:bind-buffer :element-array-buffer ,index-buffer)
     (multiple-value-prog1
	 (locally ,@body)
       ;; 0 is always reserved as an unbound object.
       (gl:bind-buffer :element-array-buffer 0))))

(defun use-array-buffer (vertex-buffer array &optional (type :static-draw))
  (bind-to-array-buffer (vertex-buffer)
    (gl:buffer-data :array-buffer type array)))
(defun use-element-array-buffer (index-buffer array &optional (type :static-draw))
  (bind-to-element-array-buffer (index-buffer)
    (gl:buffer-data :element-array-buffer type array)))

(defun make-vertex-array (verts indices layout type)
  (let* ((value (glhelp::allocate-vertex-array))
	 (vertex-array (glhelp::vertex-array value))
	 (vertex-buffer (glhelp::vertex-buffer value))
	 (index-buffer (glhelp::index-buffer value)))
    
    (let ((len (length verts)))
      (gl:with-gl-array (arr :float :count len)
	(dotimes (i len)
	  (setf (gl:glaref arr i) (aref verts i)))
	(use-array-buffer vertex-buffer arr)))
    
    ;; An element array buffer stores vertex indices. We fill it in the
    ;; same way as an array buffer.
    (let ((len (length indices)))
      (gl:with-gl-array (arr :unsigned-int :count len)
	(dotimes (i len)
	  (setf (gl:glaref arr i) (aref indices i)))
	(use-array-buffer index-buffer arr)))
    
    (associate-vbos-with-vao vertex-array vertex-buffer index-buffer layout)
    (setf (glhelp::indices value) (length indices))
    (setf (glhelp::render-type value) type)
    value))

(defun assemble-vao (gl-vertbuf gl-indexbuf layout length type)
  "assume that we have a gl-buffer for the vertices and indices, 
we know the layout, the length, and the type (points, triangles, etc...)
just put together a new vao"
  (let ((vao (make-instance 'vao))
	(vertex-array (gl:gen-vertex-array)))

    (setf (index-buffer vao) gl-indexbuf)
    (setf (vertex-buffer vao) gl-indexbuf)
    (setf (indices vao) length)
    (setf (render-type vao) type)
    (setf (vertex-array vao) vertex-array)
    (associate-vbos-with-vao vertex-array gl-vertbuf gl-indexbuf layout)
    
    vao))

(defun allocate-vertex-array ()
  (let ((vao (make-instance 'vao)))
    (setf (vertex-buffer vao)
	  (gl:gen-buffer))
    (setf (index-buffer vao)
	  (gl:gen-buffer))
    (setf (vertex-array vao)
	  (gl:gen-vertex-array))
    vao))

(defun draw-vertex-array (vao)
  (%draw-vertex-array
   (glhelp::vertex-array vao)
   (glhelp::indices vao)
   (render-type vao)))

;;;;
(deftype +gluint+ () '(unsigned-byte 32))
(declaim (inline slow-draw))
(defun slow-draw (gl-thing)
  ;;;dispatch on either display-list or vao
  ;;(declare (optimize (speed 3) (safety 0)))
  (typecase gl-thing
    (+gluint+ (glhelp::draw-display-list gl-thing))
    (vao (draw-vertex-array gl-thing))
    (gl-list (glhelp::draw-display-list (glhelp::handle gl-thing))))
  )

(defun slow-delete (gl-thing)
  ;;;dispatch on either display-list or vao
  ;;(declare (optimize (speed 3) (safety 0)))
  (typecase gl-thing
    (+gluint+ (gl:delete-lists gl-thing 1))
    (vao (delete-vao gl-thing))
    (gl-list (gl:delete-lists (glhelp::handle gl-thing) 1)))
  )
(export '(slow-draw slow-delete))

