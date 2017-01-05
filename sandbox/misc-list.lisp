(in-package :sandbox)

(defun flatten (obj)
	   (do* ((result (cons obj nil))
		 (node result))
		((null node) (delete nil result))
	     (if (consp (car node))
		 (progn (when (cdar node) 
			  (setf (cdr node) 
				(cons (cdar node) 
				      (cdr node))))
			(setf (car node) (caar node)))
		 (setf node 
		       (cdr node)))))
