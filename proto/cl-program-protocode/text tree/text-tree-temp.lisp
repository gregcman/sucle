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
    (let ((width 0)
	  (height 0))
      (labels ((rec (node)
		 (if node
		     (let ((payload (node-payload node)))		       
		       (if (funcall test payload)
			   (values node payload width height)
			   (progn
			     (incf width (payload-width payload))
			     (when (and (typep payload (quote hole))
					(hole-active payload))
			       (decf height))
			     (rec (node-next node)))))
		     nil)))
	(rec nodes))))
  (defun find-node-backward (nodes test)
    (let ((width 0)
	  (height 0))
      (labels ((rec (node)
		 (if node
		     (let ((payload (node-payload node)))
		       (if (funcall test payload)
			   (values node payload width height)
			   (progn
			     (decf width (payload-width payload))
			     (when (and (typep payload (quote hole))
					(hole-active payload))
			       (incf height))
			     (rec (node-prev node)))))
		     nil)))
	(rec nodes)))))

(progno
 (defun payload-width (payload)
   (typecase payload
     (character 1)
     (hole (if (hole-active payload)
	       (hole-width payload)
	       0))
     (t 0))))

(progno
 (defun define-indentation-rule (name &key (car *nope-generator*) (cdr *nope-generator*))
   (let ((function
	  ((lambda (parent-type)
	     (lambda (child-type &optional child)
	       (multiple-value-bind (value exists-p) (gethash parent-type *generator-graph*)
		 (if exists-p
		     (ecase child-type
		       (car (multiple-value-bind (rule exists-p)
				(gethash (funcall (car value) child) *generator-rules*)
			      (if exists-p
				  (values (car rule) (cdr rule))
				  (error "no child-type"))))
		       (cdr (multiple-value-bind (rule exists-p)
				(gethash (funcall (cdr value) child) *generator-rules*)
			      (if exists-p
				  (values (car rule) (cdr rule))
				  (error "no child-type"))))
		       (info parent-type)
		       (otherwise (error "child not car or cdr: ~a" child-type)))
		     (error "parent-type nonexistent"))))) name)))
     (setf (gethash name *generator-rules*) function)
     (setf (gethash name *generator-graph*) (cons car cdr)))))

(progno
 (defparameter *nope-generator* (lambda (node) (declare (ignore node)) (quote nope))))


