(in-package :fuktard)
(export (quote with-vec-params))
(defun with-vec-params (&rest args)
  (destructuring-bind ((&rest bufvars)
		       (buf &optional (binder 'let)) &body body) args 
    (multiple-value-bind (letargsoffset letargs decl)
	(%2aux-with-vec-params bufvars)

      (setf letargs (mapcar (lambda (x)
			      `(,(pop x) (aref ,buf ,(pop x))))
			    letargs))
      (let ((fin `(,binder ,letargs 
			   ,@body)))
	(if letargsoffset
	    `(let* ,letargsoffset
	       (declare (type fixnum ,@decl))
	       ,fin)
	    fin)))))

(defun %2aux-with-vec-params (bufvars)
  (let ((letargs nil)
	(letargsoffset nil)
	(decl nil))
    (multiple-value-bind (ans offs) (%aux-with-vec-params bufvars)
      (labels ((rec-push (stuff)
		 (dolist (x stuff)
		   (let ((first (car x)))
		     (if (consp first)
			 (rec-push x)
			 (push x letargs))))))
	(rec-push ans))
      (labels ((rec-push-offs (stuff)
		 (dolist (x stuff)
		   (let ((first (car x)))
		     (if (consp first)			 
			 (rec-push-offs x)
			 (when first (push x letargsoffset)
			       (push (car x) decl)))))))
	(rec-push-offs offs)))
    (values letargsoffset
	    letargs
	    decl)))

(defun %aux-with-vec-params (vars-or-offsets &optional (offset 0))
  (let ((counter 0)
	(bindings nil)
	(offsets nil))
    (dolist (var-or-offset vars-or-offsets)
      (if (consp var-or-offset)
	  (let ((suboffset (car var-or-offset)))
	    (multiple-value-bind (bindings-list offsets-list)
		(%aux-with-vec-params
		 (cdr var-or-offset)
		 (if (eql 0 offset)
		     suboffset
		     (let ((newoffset (gensym)))
		       (push `(,newoffset (+ ,offset ,suboffset)) offsets)
		       newoffset)))
	      (when bindings-list
		(push bindings-list bindings))
	      (when offsets-list
		(push offsets-list offsets))))
	  (progn
	    (let ((sub (if (eql 0 counter)
			   offset
			   (let ((new-offset (gensym)))
			     (push`(,new-offset (+ ,offset ,counter)) offsets)
			     new-offset))))
	      (when var-or-offset
		(push `(,var-or-offset ,sub) bindings)))
	    (incf counter))))
    (values bindings offsets)))
