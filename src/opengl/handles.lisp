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

(defun create-gl-program (src)
  (let ((inst
	 (make-instance 'gl-program :src src))
	(obj (glslgen::gl-dump-shader src)))
    (setf (handle inst) obj)
    (setf (gl-program-object-uniforms inst)
	  (cache-program-uniforms
	   obj
	   (glslgen::shader-program-data-raw-uniform src)))
    inst))

(defmethod gl-delete* ((obj gl-program))
  (gl:delete-program (handle obj)))

;;;;textures

(defclass gl-texture (gl-object)
  ())
(defmethod gl-delete* ((obj gl-texture))
  (gl:delete-textures (list (handle obj))))

;;;;call lists

(defclass gl-list (gl-object)
  ())
(defmethod gl-delete* ((obj gl-list))
  (gl:delete-lists (handle obj) 1))

;;;;vertex array objects

(defclass vao (gl-object)
  ((vbuff :accessor vertex-buffer)
   (ibuff :accessor index-buffer)
   (va :accessor vertex-array)
   (indices :accessor indices :initform 0)
   (render-type :accessor render-type :initform :triangles)))
(defmethod gl-delete* ((obj vao))
  (gl:delete-vertex-arrays (list (vertex-array obj)))
  (gl:delete-buffers (list (vertex-buffer obj) (index-buffer obj))))

(defun make-vertex-array (vertbuf indexbuf layout type)
  (let ((value (glhelp::allocate-vertex-array)))
    (glhelp::fill-vertex-array-object
     (glhelp::vertex-array value)
     (glhelp::vertex-buffer value)
     (glhelp::index-buffer value)
     vertbuf
     indexbuf
     layout)
    (setf (glhelp::indices value)
	  (length indexbuf)
	  (glhelp::render-type value)
	  type)
    value))

(defun allocate-vertex-array ()
  (let ((w (make-instance 'vao)))
    (let ((buffers (gl:gen-buffers 2)))
      (setf (vertex-buffer w) (elt buffers 0)
	    (index-buffer w) (elt buffers 1)))
    (setf (vertex-array w) (gl:gen-vertex-array))
    w))
