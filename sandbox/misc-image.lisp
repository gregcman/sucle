(in-package :sandbox)

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
