(defpackage #:imagewise
  (:use #:cl)
  (:export

   #:array-flatten
   #:getapixel
   #:flip-image
   #:load-png))
(in-package :imagewise)

;;;;Tools for the manipulation of images stored as arrays of rank 3

;;;turn a multidimensional array into a single dimensional array
;;;of the same total length
(defun array-flatten (array)
  (make-array (array-total-size array)
	      :displaced-to array
	      :element-type (array-element-type array)))

;;;;flip an image in-place - three dimensions - does not conse
(defun flip-image (image)
  (destructuring-bind (height width components) (array-dimensions image)
    (dotimes (h (- height (ash height -1)))
      (dotimes (w width)
	(dotimes (c components)
	  (rotatef (aref image (- height h 1) w c)
		   (aref image h w c))))))
  image)

;;;;load a png image from a path
(defun load-png (filename)
  (opticl:read-png-file filename))

(defun getapixel (h w image)
  (destructuring-bind (height width c) (array-dimensions image)
    (declare (ignore height))
    (make-array 4 :element-type (array-element-type image)
		:displaced-to image
		:displaced-index-offset (* c (+ w (* h width))))))

