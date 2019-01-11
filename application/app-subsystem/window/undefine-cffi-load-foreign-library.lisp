(defpackage #:undefine-cffi-load-foreign-library
  (:use :cl))
(in-package :undefine-cffi-load-foreign-library)

;;(eval-when (:compile-toplevel :load-toplevel :execute))
(defvar *cffi-load-foreign-library-function* nil)
(defvar *replace-fun* (constantly nil))
(symbol-macrolet ((cffi-fun (symbol-function 'cffi:load-foreign-library)))
  (defun undefine-cffi-load-foreign-library ()
    (let ((fun cffi-fun))
      (unless (eq fun *replace-fun*)
	(setf *cffi-load-foreign-library-function* fun)
	(setf cffi-fun *replace-fun*))))

  (defun restore-cffi-load-foreign-library ()
    (when (and *cffi-load-foreign-library-function*
	       (not (eq *cffi-load-foreign-library-function*
			*replace-fun*))) 
      (setf cffi-fun *cffi-load-foreign-library-function*))))
