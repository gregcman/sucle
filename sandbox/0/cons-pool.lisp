(in-package :sandbox)

(declaim (type list *free-cons-cells*))
(defparameter *free-cons-cells* nil)

(defun recycle-cons-cell (cell)
  (rplaca (rplacd cell *free-cons-cells*) 0)
  (setf *free-cons-cells* cell))

(defun conz (ze1 ze2)
  (if *free-cons-cells*
      (let ((cell *free-cons-cells*))
	(setf *free-cons-cells* (cdr *free-cons-cells*))
	(rplaca (rplacd cell ze2) ze1))
      (cons ze1 ze2)))
