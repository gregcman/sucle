(in-package :sandbox)

(progno
   (defun acolor (&rest values)
     (setf values (nreverse values))
     (let ((acc 0))
       (dolist (value values)
	 (setf acc (ash (logior acc value) 8)))
       (logand acc most-positive-fixnum)))

   (defun strip-char (color)
     (logandc1 255 color)))

(progno
  (defparameter *white-black-color* (acolor 255 255 255 0 0 0))
  (defparameter *color-nil* (logandc1 255 (sxhash nil))))


(progno
  (defun random-color ()
    (logandc1 255 (random most-positive-fixnum)))

  (defun color-invert (color)
    (logior (logand 255 color)
	    (logand (logxor most-positive-fixnum 255)
		    (lognot color)))))
