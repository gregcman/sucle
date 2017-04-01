(in-package :sandbox)

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
