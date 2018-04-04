;;;;;;;;;
(defmacro do-vec-params ((&rest vars) (vector &optional (binder 'let)) &body body)
  (with-gensyms (index)
    `(dobox
      ((,index 0 (length ,vector) :inc ,(length vars)))
      (with-vec ((,index ,@vars)) (,vector ,binder)
	,@body))))

;;;WARNING: flush-body takes the lexical environment where with-output-to-fun is
(defmacro with-fun-to-vec ((fun with-output-to-fun)
			   ((&rest vars) &body flush-body) &body body)
  (with-gensyms (vec)
    `(let ((,vec (make-array 0 :adjustable t :fill-pointer 0)))
       (flet ((,fun ,vars
		,@ (mapcar (lambda (x) `(vector-push-extend ,x ,vec)) vars)))
	 (macrolet ((,with-output-to-fun (&body body)
		      `(progn
			 (setf (fill-pointer ,',vec) 0)
			 ,@body
			 (do-vec-params ,',vars (,',vec)
			   ,@',flush-body))))
	   ,@body))))) 
