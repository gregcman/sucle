(defpackage :physical-keyboard
  (:use :cl :funland))

(in-package :physical-keyboard)

;;;position keys relative to each other in physical space or how close they are to
;;;proper fingers?
(progno
 (defparameter *regular-keys*
   #((0 "`1234567890-=")
     (6 "qwertyuiop[]\\")
     (7 "asdfghjkl;'")
     (9 "zxcvbnm,./")))

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

 (print
  (reduce #'min
	  (mapcar #'char-code
		  (reduce #'(lambda (x y)
			      (concatenate 'list x y))
			  (map 'list #'second *regular-keys*))))))

(eval-always
 (progn
   (defparameter *data* (make-array 128))
   (defparameter *finger-keys*
     #("5tgbnhy6"
       "4rfvmju7"
       "3edc,ki8"
       "2wsx.lo9"
       "1qaz/;p0"))

   (let ((keys *finger-keys*))
     (dotimes (x 5)
       (let ((data (aref keys x))
	     (ypos (- 5 x)))     
	 (let ((len (length data)))
	   (dotimes (index len)
	     (let ((xpos index))
	       (setf (aref *data* (char-code (aref data index)))
		     (cons xpos ypos))))))))))

(defun key-position (char)
  (aref (funland:etouq *data*) (char-code char)))
(defun code-position (code)
  (aref (funland:etouq *data*) code))



