(in-package :fuktard)

(defmacro with-vec-params ((&rest bufvars) (buf &optional offset) &body body)
  (let ((letargs nil)
	(counter 0))
    (dolist (sym bufvars)	
      (push (if (consp sym)
		`(,(pop sym) (aref ,buf (the fixnum (+ ,offset ,(pop sym)))))  
		(prog1
		    `(,sym (aref ,buf (the fixnum (+ ,offset ,counter))))
		  (incf counter))) letargs))
    `(let ,letargs 
       ,@body)))

(export 'with-vec-params)
