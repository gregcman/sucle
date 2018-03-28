(defmacro macrontinue (cont-list sub-form)
  (if cont-list
      (let ((cont (pop cont-list)))
	(let ((head (car cont))
	      (tail (cdr cont)))
	  (let ((val (list sub-form tail head)))
	    (if cont-list
		(push cont-list val))
	    (nreverse val))))
      sub-form))

(defmacro orcam ((subformvar contvar) &optional body cont)
  (list (quote macrontinue) cont
	(let ((tail-var (gensym)))
	  (block orcam
	    (destructuring-bind (def name parm &rest rest) body
		(multiple-value-bind
		      (bod decl doc)
		    (utility:parse-body rest)
		  `(,def ,name (,parm &optional ,subformvar ,contvar) 
		     ,@decl
		     (declare (ignorable ,subformvar ,contvar))
		     ,doc
		     (let ((,tail-var ,(list* (quote block) name bod)))
		       (list (quote macrontinue)
			     ,contvar
			     ,tail-var)))))))))

(defmacro chain (input &rest conts)
  `(macrontinue ,conts ,input))

(orcam (subform cont)
       (defmacro mist ()
	 (apply #'list* subform)))

(orcam (subform cont)
       (defmacro stim (&optional form) form))

(orcam (subform cont)
       (defmacro peach (func-or-macro)
	 (mapcar (lambda (x) (list func-or-macro x)) subform)))

(orcam (subform cont)
       (defmacro warp (&rest func-or-macro)
	 (append func-or-macro subform)))

(orcam (subform cont)
       (defmacro croam () subform))

(orcam (subform cont)
       (defmacro sublock (&rest conts)
	 (dolist (x (nreverse conts))
	   (push x cont))
	 subform))

(orcam (subform cont)
       (defmacro reps (size)
	 (make-list size :initial-element subform)))

(orcam (subform cont)
       (defmacro lave ((subvar contvar) &body body)
	 (multiple-value-bind (bod dec doc) (utility:parse-body body)
	   (declare (ignorable doc))
	   (multiple-value-bind (sub con)
	       (eval (let ((val-var (gensym)))
		       `(let ((,subvar ,subform)
			      (,contvar (quote ,cont)))
			  ,@dec
			  (let ((,val-var (block nil ,@bod)))
			    (values ,val-var ,contvar)))))
	     (setf cont con)
	     sub))))

(chain (macrontinue orcam chain mist stim peach warp croam sublock reps lave)
       (peach quote)
       (peach export)
       (warp progn))
