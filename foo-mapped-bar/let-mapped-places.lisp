(defpackage #:foo-mapped-bar
  (:use :cl
	:declaration-generation)
  (:export
   #:with-let-mapped-places))

(in-package #:foo-mapped-bar)

(defmacro with-let-mapped-places ((&rest place-pairs) &body body)
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
    `(let ,let-args
       ,(cons 'declare
	      (mapcar (lambda (x)
			(cons 'type x))
		      type-multimap-alist))
       (multiple-value-prog1
	   ,(cons 'progn body)
	 ,(cons 'setf setf-args)))))
