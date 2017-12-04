(in-package :fuck)

(defparameter *some-saves* 
  #P "/home/imac/Documents/lispysaves/saves/sandbox-saves/")
(defun call-with-path (path fun)
  (let ((newpath (merge-pathnames path *some-saves*)))
    (cond ((or (uiop:pathname-equal newpath *some-saves*) 
	       (not (uiop:subpathp newpath *some-saves*))
	       (not (uiop:directory-pathname-p newpath)))
	   (error "SAVE DIR INVALID: ~a, ~a" path newpath))
	  (t
	   (ensure-directories-exist newpath)
	   (funcall fun newpath)))))
(defun msave (path)
  (call-with-path path #'sandbox::save-world))
(defun mload (path)
  (call-with-path path #'sandbox::load-world))

