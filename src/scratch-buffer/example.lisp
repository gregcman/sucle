(defpackage #:reverse-array-array-example
  (:use #:cl
	#:reverse-array-iterator
	#:reverse-array-array))

(in-package :reverse-array-array-example)

(defparameter *raa* (make-raa))
(defun test ()
  (with-raa-iterator (next place *raa*)
    (dotimes (x 3000)
      (next)
      (setf place x)))
  (let ((iterator (make-raa-iterator *raa*)))
    (bind-iterator-in (deref t) iterator
		      (dotimes (x 3000)
			(assert (= x (deref)))))
    (reset-iterator iterator)
    (bind-iterator-out (emit t) iterator
		       (dotimes (x 3000)
			 (emit (- x 255)))))
  (with-raa-iterator (next place *raa*)
    (dotimes (x 3000)
      (next)
      (assert (= (- x 255) place)))))
