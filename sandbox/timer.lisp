(in-package :sandbox)

(defun timer ()
  "control the fps execution"
  (let ((prevtime (fine-time)))
    (lambda (time afunc)
      (let* ((now (fine-time))
	     (diff (- now prevtime)))
	(if (> diff time)
	    (progn
	      (setf prevtime now)
	      (funcall afunc)
	      diff)
	    nil)))))

(defun fine-time ()
  (multiple-value-bind (s m) (sb-ext:get-time-of-day)
    (+ (* (expt 10 6) s) m)))
