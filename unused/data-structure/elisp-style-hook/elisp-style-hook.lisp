(defpackage #:elisp-style-hook
  (:use #:cl)
  (:export
   #:create-hook
   #:add-hook
   #:remove-hook
   #:run-hook
   #:run-hooks
   #:run-hook-with-args
   #:run-hook-with-args-until-failure
   #:run-hook-with-args-until-success))

(in-package :hook)

(defun create-hook ()
  (make-hash-table :test 'eql))

(defun add-hook (hook name function)
  (setf (gethash name hook) function))

(defun remove-hook (hook name)
  (remhash name hook))

(defun run-hook (hook)
  (maphash (lambda (k v)
	     (declare (ignorable k))
	     (funcall v))
	   hook))

(defun run-hooks (&rest hookvars)
  (dolist (hook hookvars)
    (run-hook hook)))

(defun run-hook-with-args (hook &rest args)
  (maphash (lambda (k v)
	     (declare (ignorable k))
	     (apply v args))
	   hook))

(defun run-hook-with-args-until-failure (hook &rest args)
  (maphash (lambda (k v)
	     (declare (ignorable k))
	     (let ((ans (apply v args)))
	       (unless ans
		   (values ans t))))
	   hook)
  (values nil nil))

(defun run-hook-with-args-until-success (hook &rest args)
  (block nil
    (maphash (lambda (k v)
	       (declare (ignorable k))
	       (let ((ans (apply v args)))
		 (when ans
		   (return (values ans t)))))
	     hook))
  (values nil nil))

