(defpackage :struct-to-clos
  (:use #:cl #:utility)
  (:export #:struct->class))

(in-package :struct-to-clos)

(defun conc-name (title)
  (concatenate 'string (string title) "-"))
(defun keywordify (string-designator)
  (intern (string string-designator)
	  (load-time-value (find-package "KEYWORD"))))

(defmacro struct->class (form)
  (%struct->class form))

(defun %struct->class (form)
  (destructuring-bind (header title &rest slots) form
    (declare (ignore header))
    (unless (symbolp title)
      (setf title (first title)))
    (let ((conc-name (conc-name title))
	  (new-slots ()))
      (flet ((add-slot (name &optional value)
	       (push `(,name :initform ,value
			     :initarg ,(keywordify name)
			     :accessor ,(symbolicate2 (list conc-name name)))
		     new-slots)))
	(dolist (slot slots)
	  (etypecase slot
	    (symbol (add-slot slot))
	    (list (destructuring-bind (name initform &rest ignorable) slot
		    (declare (ignorable ignorable))
		    (add-slot name initform))))))
      `(progn
	 (defclass ,title ()
	   ,new-slots)
	 (defun ,(symbolicate2 (list conc-name "P")) (object)
	   (typep object ',title))
	 (defun ,(symbolicate2 (list "MAKE-" title)) (&rest rest &key &allow-other-keys)
	   (apply #'make-instance ',title rest))))))
#+nil
(defstruct a
  (a 0 :type t)
  b)

#+nil
(defclass a () 
  ((a :initform 0
      :initarg :a
      :accessor a-a)
   (b :initform nil
      :initarg :b
      :accessor a-b)))
#+nil
(defun a-p (object)
  (typep object 'a))
#+nil
(defun make-a (&rest rest &key &allow-other-keys)
  (apply #'make-instance 'a rest))
  
