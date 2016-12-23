(in-package :macrology)

;;;toggle a place between true and nil
(defmacro toggle (var)
  `(setf ,var (not ,var)))

;;;comment out code the macro way!
(defmacro progno (&rest args))

;;;like dotimes, but for a range, and without a return
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

;;;when the only let value is returned at the end
(defmacro ret (var bound-form &body body)
  `(let ((,var ,bound-form))
     ,@body
     ,var))

;;;used to rename something
(defmacro rename (&rest rebind-forms)
  (let (acc)
    (dolist (x rebind-forms)
      (push `(defmacro ,(second x) (&body body)
	       (cons ',(car x) body)) acc))
    (cons 'progn acc)))

;;defparamter dp is rotationally symmetric
;;lambda gave rise to the latin l
;;multiple-value-bind is really fucking long
(rename (defparameter dp)
	(lambda l)
	(multiple-value-bind mvb))
