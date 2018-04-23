(defpackage #:filesystem-util
  (:use :cl
	:utility)
  (:export save myload save2 myload2 rebase-path)
  (:export this-file this-directory))
(in-package #:filesystem-util)

(defun save (path things)
  (with-open-file (stream path :direction :output :if-does-not-exist :create :if-exists :supersede)
    (dolist (thing things)
      (print thing stream))))

(defun myload (path)
  (let ((things nil))
    (with-open-file (stream path :direction :input :if-does-not-exist nil)
      (tagbody rep
	 (let ((thing (read stream nil nil)))
	   (when thing
	     (push thing things)
	     (go rep)))))
    (nreverse things)))

(defun save2 (path thingfilename &rest things)
  (save (merge-pathnames (format nil "~s" thingfilename) path) things))
(defun myload2 (path thingfilename)
  (myload (merge-pathnames (format nil "~s" thingfilename) path)))

(defun rebase-path (path base)
  (let ((newpath (merge-pathnames path base)))
    (cond ((or (uiop:pathname-equal newpath base) 
	       (not (uiop:subpathp newpath base)))
	   (error "not a subpath
~a
~a" path newpath))
	  (t
	   newpath))))

(defmacro this-file ()
  `(etouq (or *compile-file-truename*
	      *load-truename*)))

(eval-always
  (defun file-directory (value)
    (make-pathname :host (pathname-host value)
		   :directory (pathname-directory value))))

(defmacro this-directory ()
  `(etouq (file-directory (this-file))))
