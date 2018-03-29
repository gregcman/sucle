(defun valid-list-p (list)
  (let ((node list))
    (tagbody rep
       (if (consp node)
	   (progn
	     (setf node (cdr node))
	     (go rep))
	   (return-from valid-list-p (null node)))))) 
