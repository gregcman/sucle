(defpackage #:control
  (:use #:cl #:utility)
  (:export
   #:wasd-mover
   #:num-key-jp))

(in-package #:control)

(defun wasd-mover (w? a? s? d?)
  (let ((acc 0))
    (flet ((add (var bit)
	     (when var
	       (setf acc (logior acc bit)))))
      (add w? #b0001)
      (add a? #b0010)
      (add s? #b0100)
      (add d? #b1000))   
    (aref (etouq (let ((array (make-array (expt 2 4))))
		   (dotimes (index (length array))
		     (symbol-macrolet ((w? (logbitp 0 index))
				       (a? (logbitp 1 index))
				       (s? (logbitp 2 index))
				       (d? (logbitp 3 index)))
		       (let ((x 0)
			     (y 0))
			 (when w? (decf x))
			 (when a? (decf y))
			 (when s? (incf x))
			 (when d? (incf y))
			 (if (and (zerop x)
				  (zerop y))
			     (setf (aref array index) nil)
			     (setf (aref array index)
				   (floatify (atan y x)))))))
		   array))
	  acc)))

(defun num-key-jp (&optional (control-state window::*control-state*))
  (etouq
   (cons
    'cond
    (mapcar
     (lambda (n)
       `((window::skey-j-p
	  (window::keyval ,n)
	  control-state) ,n))
     '(0 1 2 3 4 5 6 7 8 9)))))
 
