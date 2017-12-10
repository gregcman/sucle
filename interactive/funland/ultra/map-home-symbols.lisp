(in-package :funland)
(eval-always
 (defun map-home-symbols (package func)
   (do-symbols (symbol package)
     (let ((home (symbol-package symbol)))
       (when (eql package home)
	 (funcall func symbol))))))

(eval-always
 (defun export-all-functions-and-symbols (&optional (package-designator *package*))
   (map-home-symbols (find-package package-designator)
		     (lambda (x)
		       (if (or (boundp x)
			       (fboundp x))
			   (export x))))))

(eval-always (export-all-functions-and-symbols :funland))
