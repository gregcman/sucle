(defpackage #:flip-image
  (:use #:cl))

(in-package #:flip-image)

;;;;destructive flip
(defun flip-image (image)
  (let ((dims (array-dimensions image)))
    (let ((height (pop dims))
	  (longjumps (reduce #'* dims)))
      (declare (type fixnum height longjumps))
      (let ((magic (* longjumps (- height 1))))
	(loop for h below (* longjumps (- height (floor height 2))) by longjumps do
	     (loop for w below longjumps do
		  (rotatef (row-major-aref image (+ (- magic h) w))
			   (row-major-aref image (+ h w))))))))
  image)

(export '(flip-image))
