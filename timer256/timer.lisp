(in-package :timer)

;;;timer: fire function only after a certain interval
;;;the interval is the time
;;;if the function fire, returns the value of the function, whether it actually
;;;happened, the difference, the current time

(defun timer ()
  (let ((prevtime (fine-time)))
    (lambda (time afunc)
      (block nil
	(let* ((now (fine-time))
	       (diff (- now prevtime)))
	  (when (> diff time)
	    (setf prevtime now)
	    (return (values (funcall afunc) t diff now)))
	  (values nil nil diff now))))))

(defun fine-time ()
  (multiple-value-bind (s m) (sb-ext:get-time-of-day)
    (+ (* (expt 10 6) s) m)))

