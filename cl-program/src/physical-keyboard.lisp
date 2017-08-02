(defpackage :physical-keyboard
  (:use :cl))

(in-package :physical-keyboard)

;;;position keys relative to each other in physical space or how close they are to
;;;proper fingers?
(defparameter *regular-keys*
  #((0 "`1234567890-=")
    (6 "qwertyuiop[]\\")
    (7 "asdfghjkl;'")
    (9 "zxcvbnm,./")))

(print
 (reduce #'min
	 (mapcar #'char-code
		 (reduce #'(lambda (x y)
			     (concatenate 'list x y))
			 (map 'list #'second *regular-keys*)))))

(defparameter *data* (make-array 128))

(let ((keys *regular-keys*))
  (dotimes (x 4)
    (let ((list (aref keys x))
	  (ypos (* 4 (- 4 x))))     
      (destructuring-bind (offset data) list
	(let ((len (length data)))
	  (dotimes (index len)
	    (let ((xpos (+ offset (* index 4))))
	      (setf (aref *data* (char-code (aref data index)))
		    (cons xpos ypos)))))))))

(defun key-position (char)
  (aref (fuktard:etouq *data*) (char-code char)))
(defun code-position (code)
  (aref (fuktard:etouq *data*) code))
