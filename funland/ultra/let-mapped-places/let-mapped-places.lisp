(in-package :fuktard)

(defun type-multimap-alist (type varname alist)
  (let ((value (assoc type alist :test 'equal)))
    (if value
	(push varname (cdr value))
	(push (list type varname) alist))
    alist))

(defmacro with-let-mapped-places ((&rest place-pairs) &body body)
  (%with-let-mapped-places place-pairs body))

(export (quote %with-let-mapped-places))
(defun %with-let-mapped-places (place-pairs &optional body)
  (let ((let-args nil)
	(setf-args nil)
	(type-multimap-alist nil))
    (dolist (place-pair place-pairs)
      (let ((reg-place (pop place-pair))
	    (ram-place (pop place-pair))
	    (type (pop place-pair)))
	(push (list reg-place ram-place) let-args)
	(push reg-place setf-args)
	(push ram-place setf-args)
	(if type
	    (setf type-multimap-alist (type-multimap-alist type reg-place type-multimap-alist)))))
    (multiple-value-bind (new-body decl) (alexandria:parse-body body)
      `(let ,let-args
	 ,(cons 'declare
		(mapcar (lambda (x)
			  (cons 'type x))
			type-multimap-alist))
	 ,@decl
	 (multiple-value-prog1
	     ,(cons 'progn new-body)
	   ,(cons 'setf setf-args))))))
