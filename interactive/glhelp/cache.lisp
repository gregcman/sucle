(in-package :glhelp)

(defun cache-program-uniforms (program uniforms)
  (mapcar
   (lambda (args)
     (destructuring-bind (id . string) args
       (cons
	id
	(gl:get-uniform-location program string))))
   uniforms))
(defun getuniform (shader-info name)
  (cdr (assoc name shader-info :test 'eq)))
(defmacro with-uniforms (name uniforms &body body)
  `(macrolet ((,name (id)
		`(getuniform ,',uniforms ,id)))
     . ,body))

(export '(getuniform cache-program-uniforms make-uniform-cache with-uniforms))
