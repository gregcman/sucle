(in-package #:sandbox)

(progn
  (defparameter *g/image* (make-hash-table :test 'equal))		    ;;raw image arrays
  (defun get-image (name)
    (let ((img (gethash name *g/image*)))
      (if img
	  img
	  (get-image-backup name))))
  (defun set-image (name image-data)
    (setf (gethash name *g/image*) image-data))
  (defun remove-image (name)
    (remhash name *g/image*)))
(progn
  (defparameter *g/image-backup* (make-hash-table :test 'equal))
  (defun get-image-backup (name)
    (let ((image-func (gethash name *g/image-backup*)))
      (when (functionp image-func)
	(let ((ans (funcall image-func name)))
	  (when ans
	    (set-image name ans)))))))

(progn
  (defparameter *g/text* (make-hash-table :test 'equal))   ;;text: sequences of bytes
  (defun get-text (name)
    (let ((text (gethash name *g/text*)))
      (if text
	  text
	  (get-text-backup name))))
  (defun set-text (name text-data)
    (setf (gethash name *g/text*) text-data))
  (defun remove-text (name)
    (remhash name *g/text*)))
(progn
  (defparameter *g/text-backup* (make-hash-table :test 'equal))
  (defun get-text-backup (name)
    (let ((text-func (gethash name *g/text-backup*)))
      (when (functionp text-func)
	(let ((ans (funcall text-func name)))
	  (when ans
	    (set-text name ans)))))))
