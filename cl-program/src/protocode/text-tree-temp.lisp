(in-package :sandbox)

(progno
 (defun test420 ()
  (map-nodes barfoo
	     (lambda (x)
	       (let ((payload (node-payload x)))
		 (typecase payload
		   (hole (setf (hole-active payload)
			       (if (zerop (random 2)) t nil)
			       (hole-width payload)
			       (random 10)))))))))

(progno
;;;walking
 (defun draw-nodes (x y nodes)
   (let ((cap 512))
     (dotimes (index cap)
       (scwu nodes x y)
       (let ((next (node-next-char (node-next nodes))))
	 (if next
	     (progn
	       (incf x)
	       (setf nodes next))
	     (return)))))))

(progno
 ;;walking
 (defun map-nodes (nodes function &optional (times 512))
   (dotimes (x times)
     (funcall function nodes)
     (setf nodes (node-next nodes)))))

(progno
 (defun bracket-type (data)
   (when (or (eq data (quote left))
	     (eq data (quote right)))
     data)))

(progno
;;;walking
 (defun node-next-char (node)
   (if node
       (if (typep (node-payload node) (quote character))
	   node
	   (node-next-char (node-next node)))))

 ;;walking
 (defun node-prev-char (node)
   (if node
       (if (typep (node-payload node) (quote character))
	   node
	   (node-prev-char (node-prev node))))))

(progno
  (declaim (ftype (function (node (function (t) t)) (or null node))
		  find-node-forward find-node-backward))
  (defun find-node-forward (nodes test)
    (labels ((rec (node)
	       (if node
		   (let ((payload (node-payload node)))
		     (if (funcall test payload)
			 (values node payload)
			 (rec (node-next node))))
		   nil)))
      (rec nodes)))
  (defun find-node-backward (nodes test)
    (labels ((rec (node)
	       (if node
		   (let ((payload (node-payload node)))
		     (if (funcall test payload)
			 (values node payload)
			 (rec (node-prev node))))
		   nil)))
      (rec nodes))))
