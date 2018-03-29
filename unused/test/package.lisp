
(eval-always
  (defun map-package (fun &optional (package *package*))
    (let ((package (find-package package)))
      (do-symbols (sym package)
	(when (eq package
		  (symbol-package sym))
	  (funcall fun sym))))))
(defun vars (&optional (fun #'print) (package *package*))
  (map-package (lambda (x)
		 (when (boundp x)
		   (funcall fun x)))
	       package))

(defun funs (&optional (fun #'print) (package *package*))
  (map-package (lambda (x)
		 (when (fboundp x)
		   (funcall fun x)))
	       package))

(eval-always
 (defun export-all-functions-and-symbols (&optional (package-designator *package*))
   (map-package (find-package package-designator)
		     (lambda (x)
		       (if (or (boundp x)
			       (fboundp x))
			   (export x))))))
