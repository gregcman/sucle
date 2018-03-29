(in-package :sandbox)

(defparameter default-indentation-info (make-hash-table :test 'eq))
(defun seed-default-indentation-info ()
  )

(defun newline-indent (indent-number &optional (stream *standard-output*))
    (write-char #\newline stream)
    (dotimes (x indent-number)
      (write-char #\Space stream)))

(defun valid-list-p (list)
  (let ((node list))
    (tagbody rep
       (if (consp node)
	   (progn
	     (setf node (cdr node))
	     (go rep))
	   (return-from valid-list-p (null node))))))

(defun print-program (code &optional (indentation-info default-indentation-info) (stream *standard-output*))
  (labels ((rep (node indentation)
	     (if (consp node)
		 (let ((car (car node)))
		   (if (symbolp car)
		       ()
		       (prin1 car)))))
	 )
    (rep code 0)))

