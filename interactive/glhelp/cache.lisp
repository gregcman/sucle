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
  (let ((uniforms-var (gensym)))
    `(let ((,uniforms-var ,uniforms))
       (macrolet ((,name (id)
		    (list 'getuniform ',uniforms-var id)))
	 . ,body))))

(export '(getuniform cache-program-uniforms make-uniform-cache with-uniforms))
