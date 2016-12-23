(in-package :macrology)

(defmacro toggle (var)
  `(setf ,var (not ,var)))

(defmacro progno (&rest args))

(defmacro dorange ((var start length) &rest body)
  (let ((temp (gensym))
	(temp2 (gensym))
	(tempstart (gensym))
	(templength (gensym)))
    `(block nil
       (let* ((,templength ,length)
	      (,tempstart ,start)
	      (,var ,tempstart))
	 (declare (type signed-byte ,var))
	 (tagbody
	    (go ,temp2)
	    ,temp
	    (tagbody ,@body)
	    (psetq ,var (1+ ,var))
	    ,temp2
	    (unless (>= ,var (+ ,tempstart ,templength)) (go ,temp))
	    (return-from nil (progn nil)))))))
