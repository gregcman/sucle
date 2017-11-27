(in-package :glhelp)

(defun make-uniform-cache ()
  (make-hash-table :test 'eq))
(defun cache-program-uniforms (program table args)
  (dolist (arg args)
    (setf (gethash (car arg) table)
	  (gl:get-uniform-location program (cdr arg)))))
(defun getuniform (shader-info name)
  (gethash name shader-info))

(export '(getuniform cache-program-uniforms make-uniform-cache))
