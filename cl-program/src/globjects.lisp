(in-package :aplayground)

(defparameter *opengl-objects-regular*
  '(:buffer
    :query
    :sampler
    :texture
    :renderbuffer))

(defparameter *opengl-objects-container*
  '(:framebuffer
    :program-pipeline
    :transform-feedback
    :vertex-array))

(defparameter *opengl-objects-non-standard*
  '(:shader
    :program))

(defparameter *opengl-objects-other*
  '(:display-list))

;;;not included: sync objects?

(defun make-globject-container ()
  (make-hash-table :test (quote eq)))

;;;regular objects

(defun make-meta-globject ()
  (let ((table (make-globject-container)))
    (flet ((put (x)
	     (setf (gethash x table) (make-globject-container))))
      (dolist (x *opengl-objects-regular*)
	(put x))
      (dolist (x *opengl-objects-container*)
	(put x))
      (dolist (x *opengl-objects-non-standard*)
	(put x))
      (dolist (x *opengl-objects-other*)
	(put x)))
    table))

(defun reset (gl-objects-container)
  (maphash (lambda (k v)
	     (declare (ignorable k))
	     (clrhash v))
	   gl-objects-container))


(defparameter *gl-objects* (make-meta-globject))

(defparameter *gl-texture* (gethash :texture *gl-objects*))
(defparameter *gl-shader* (gethash :shader *gl-objects*))
(defparameter *gl-program* (gethash :program *gl-objects*))
(defparameter *gl-display-list* (gethash :display-list *gl-objects*))

(defun glget (globject type)
  (gethash globject (gethash type *gl-objects*)))
(defun register (globject type &optional (data globject))
  (setf (gethash globject (gethash type *gl-objects*)) data))
(defun unregister (globject type)
  (remhash globject (gethash type *gl-objects*)))

(defun glgentextures (count)
  (let ((textures (gl:gen-textures count)))
    (dolist (texture textures)
      (register texture :texture))
    textures))
(defun gldeletetextures (textures)
  (dolist (texture textures)
    (unregister texture :texture)
    (gl:delete-textures textures)))

(defun glcreateprogram ()
  (let ((program (gl:create-program)))
    (register program :program)
    program))
(defun gldeleteprogram (program)
  (unregister program :program)
  (gl:delete-program program))
(defun glcreateshader (type)
  (let ((shader (gl:create-shader type)))
    (register shader :shader)
    shader))
(defun gldeleteshader (shader)
  (unregister shader :shader)
  (gl:delete-shader shader))
