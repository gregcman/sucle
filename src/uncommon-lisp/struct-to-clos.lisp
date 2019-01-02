(defpackage :struct-to-clos
  (:use #:cl #:utility)
  (:export #:struct->class))

(in-package :struct-to-clos)

(defun conc-name (title)
  (concatenate 'string (string title) "-"))

(defmacro struct->class (form)
  (%struct->class form))

;;;;see http://www.lispworks.com/documentation/lw51/CLHS/Body/m_defstr.htm
(defun get-conc-name (name-and-options)
  (cond ((find :conc-name name-and-options) "")
	(t (let ((cell (find-if (lambda (x)
				  (and (listp x)
				       (eq :conc-name (first x))))
				name-and-options)))
	     (if cell
		 (let ((name (second cell)))
		   (if name
		       name
		       ""))
		 nil)))))

(defun title-from-name-and-options (name-and-options)
  (cond ((symbolp name-and-options)
	 name-and-options)
	(t (first name-and-options))))

(defun %struct->class (form)
  (destructuring-bind (header name-and-options &rest slots) form
    (declare (ignore header))
    (let ((title (title-from-name-and-options name-and-options)))
      (let ((conc-name (or (and (listp name-and-options)
				(get-conc-name name-and-options))
			   (conc-name title)))
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
	   (defun ,(symbolicate2 (list title "-P")) (object)
	     (typep object ',title))
	   (defun ,(symbolicate2 (list "MAKE-" title)) (&rest rest &key &allow-other-keys)
	     (apply #'make-instance ',title rest)))))))
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
  
