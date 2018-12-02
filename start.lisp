(defpackage #:start
  (:use #:cl)
  (:export
   #:this-directory))
(in-package #:start)
(defmacro this-directory ()
  `(macrolet ((etouq (&body body)
		     (let ((var (gensym)))
		       `(macrolet ((,var () ,@body))
			  (,var)))))
     (etouq (let ((value (or *compile-file-truename*
			     *load-truename*)))
	      (make-pathname :host (pathname-host value)
			     :directory (pathname-directory value))))))

(defparameter *this-directory* (this-directory))

(require 'asdf)
;; An alternative to the standard sysdef search can be defined.  This                
;; code below can be dropped into your Lisp init files and customized.               
;; It will search for all ASDF systems in subdirectories of the                      
;; specified directories.  That lets you simply "drop-in" new packages               
;; into one of the specified directories, and it will be available for               
;; loading without any further steps.                                                
(in-package #:asdf)
(defparameter *subdir-search-registry*
  (list start::*this-directory*)
  "List of directories to search subdirectories within.")
(defparameter *subdir-search-wildcard* :wild-inferiors
  "Value of :wild means search only one level of subdirectories; value of :wild-inferiors means search
 all levels of subdirectories (I don't advise using this in big directories!)")
(defun sysdef-subdir-search (system)
  (let ((latter-path (make-pathname :name (coerce-name system)
                                    :directory (list :relative
                                                     *subdir-search-wildcard*)
                                    :type "asd"
                                    :version :newest
                                    :case :local)))
    (dolist (d *subdir-search-registry*)
      (let* ((wild-path (merge-pathnames latter-path d))
             (files (directory wild-path)))
        (when files
          (return (first files)))))))
(pushnew 'sysdef-subdir-search *system-definition-search-functions*)

(load (merge-pathnames "foreign/quicklisp/bundle/bundle.lisp"
		       start::*this-directory*))

(asdf:load-system "trivial-features")
(asdf:load-system "cffi")

(load (merge-pathnames "foreign/lib/bin.lisp"
		       start::*this-directory*))
