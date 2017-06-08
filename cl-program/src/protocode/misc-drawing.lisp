(in-package :sandbox)

(progno
 (defun print-page (x y)
   (let ((array (gethash (pix:xy-index x y)
			 sandbox::*chunks*)))
     (if array
	 (let ((fin (make-array (+ 16 (* 16 16)) :element-type 'character)))
	   (let ((counter 0))
	     (dotimes (y 16)
	       (progn (setf (aref fin counter) #\Newline)
		      (incf counter))
	       (dotimes (x 16)
		 (let ((value (aref array (+ x (ash y 4)))))
		   (setf (aref fin counter)
			 (if value
			     (code-char (mod (get-char-num value) 256))
			     #\Space)))
		 (incf counter))))
	   fin)))))

(progno
 (defun chunk-update (x y world)
   (multiple-value-bind (chunk offset) (pix::area x y world)
     (setf (aref chunk (* 16 16)) *ticks*))))
