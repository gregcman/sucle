(in-package :fuktard)
(export (quote (with-vec-params with-vec-params2)))
(defun with-vec-params (&rest args)
  (destructuring-bind ((&rest bufvars)
		       (buf &optional (binder 'let)) &body body) args
    (let ((param-data (with-vec-params2 bufvars)))
      (let ((last (cdr (assoc :last param-data)))
	    (letargs (cdr (assoc :letargs param-data))))
	(let ((new-let-args (mapcar
			     (lambda (x)
			       `(,(pop x) (aref ,buf ,(pop x))))
			     letargs)))
	  (let ((new-last
		 `(,binder ,new-let-args ,@body)))
	    (setf (cdr last) (list new-last)))))
      (cdr (assoc :head param-data)))))

(defun with-vec-params2 (bufvars)
  (multiple-value-bind (letargsoffset letargs decl)
      (%2aux-with-vec-params bufvars)

    (let ((fin `(let* ,letargsoffset
		  (declare (type fixnum ,@decl)))))
      (let ((last (last fin)))
	(pairlis (quote (:head
			 :last
			 :letargs))
		 (list fin
		       last
		       letargs))))))

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
