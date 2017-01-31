(in-package :sandbox)

(declaim (type list *free-cons-cells*))
(defparameter *free-cons-cells* nil)

(defun recycle-cons-cell (cell)
  (rplaca (rplacd cell *free-cons-cells*) 0)
  (setf *free-cons-cells* cell))

(declaim (inline conz))
(defun conz (ze1 ze2)
  (locally (declare (optimize (speed 3) (safety 0)))
    (let ((cell *free-cons-cells*))
      (if cell
	  (progn
	    (setf *free-cons-cells* (cdr cell))
	    (rplaca (rplacd cell ze2) ze1))
	  (cons ze1 ze2)))))
