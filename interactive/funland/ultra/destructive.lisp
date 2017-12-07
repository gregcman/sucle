(in-package :fuktard)

(defmacro %list (target &rest args)
  (let ((list-var (gensym))
	(original (gensym)))
    `(let* ((,list-var ,target)
	    (,original ,list-var))
       ,@(let (a
	       (first? t))
	      (dolist (arg args)
		(if first?
		    (setf first? nil)
		    (push `(setf ,list-var (cdr ,list-var)) a))
		(push `(setf (car ,list-var) ,arg) a))
	      (nreverse a))
       ,original)))

(export '(%list))
