(in-package :sandbox)

(defparameter *ourdir-aux* #.(or *compile-file-truename*
				 *load-truename*))
(defparameter *ourdir*
  (let ((value *ourdir-aux*))
    (make-pathname :host (pathname-host value)
		   :directory (pathname-directory value))))
(defun sub-path (name)
  (merge-pathnames name *ourdir*))
