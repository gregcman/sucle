(in-package :fuktard)
(export (quote with-vec-params))
(defmacro with-vec-params ((&rest bufvars) (buf) &body body)
  (let ((letargs nil)
	(ans (%aux-with-vec-params bufvars buf)))
    (labels ((rec-push (stuff)
	     (dolist (x stuff)
	       (let ((first (car x)))
		 (if (atom first)
		     (push x letargs)
		     (rec-push x))))))
      (rec-push ans))
    `(let ,letargs 
       ,@body)))

(defun %aux-with-vec-params (vars-or-offsets buf &optional (offset 0))
  (let ((counter 0)
	(acc nil))
    (dolist (var-or-offset vars-or-offsets)
      (if (consp var-or-offset)
	  (let ((suboffset (car var-or-offset)))
	    (push (%aux-with-vec-params
		   (cdr var-or-offset)
		   buf
		   (if (zerop offset)
		       suboffset
		       `(the fixnum (+ ,offset ,suboffset))))
		  acc))
	  (progn
	    (let ((sub (if (= 0 counter)
			   offset
			   `(the fixnum (+ ,offset ,counter)))))
	      (when var-or-offset
		(push `(,var-or-offset (aref ,buf ,sub)) acc)))
	    (incf counter))))
    acc))
