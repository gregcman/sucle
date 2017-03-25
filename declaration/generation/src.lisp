(defpackage #:declaration-generation
  (:use #:cl)
  (:nicknames #:decgen)
  (:export
   #:type-multimap-alist))

(in-package :declaration-generation)

(defun type-multimap-alist (type varname alist)
  (let ((value (assoc type alist :test 'equal)))
    (if value
	(push varname (cdr value))
	(push (list type varname) alist))
    alist))
