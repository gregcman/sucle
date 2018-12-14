(defpackage #:image-utility
  (:use #:cl)
  (:export
   #:flip-image)
  (:export
   #:*flip-image-p*
   #:read-png-file
   #:write-png-file))

(in-package #:image-utility)

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

(defparameter *flip-image-p* nil)
(defun read-png-file (pathname &optional (flip-p *flip-image-p*))
  (let ((array
	 (opticl:read-png-file pathname)))
    (if flip-p
	(flip-image array)
	array)))

(defun write-png-file (pathname image)
  (opticl:write-png-file pathname image))
