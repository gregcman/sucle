(in-package :funland)

;;uiop:parse-body alexandria parse-body?
(eval-always
 (defun destructure-def (form)
   (destructuring-bind (header name params &rest rest) form
     (multiple-value-bind
	   (declares documentation body)
	 (destructure-body rest)
       (values header name params declares documentation body)))))

(eval-always
 (defun form-declare-p (form)
   (if (consp form)
       (eq (car form) (quote declare)))))

(eval-always
 (defun destructure-body (body)
   (let ((in-body nil)
	 (documentation nil)
	 (declares nil))
     (let ((post-decs  (block declares-over
			 (do ((form body (cdr form)))
			     ((null form))
			   (let ((aform (car form)))
			     (if (form-declare-p aform)
				 (push aform declares)
				 (return-from declares-over form)))))))
       (let ((after-decs (car post-decs)))
	 (if (stringp after-decs)
	     (let ((end (cdr post-decs)))
	       (if end
		   (progn
		     (setf documentation after-decs)
		     (setf in-body end))
		   (setf in-body post-decs)))
	     (setf in-body post-decs))))
     (values declares documentation in-body))))
