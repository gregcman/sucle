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
    `(let ((,uniforms-var (gl-program-object-uniforms ,uniforms)))
       (macrolet ((,name (id)
		    (list 'getuniform ',uniforms-var id)))
	 . ,body))))

(export '(getuniform cache-program-uniforms make-uniform-cache with-uniforms))

;;wrapper
(defclass gl-program-object ()
  ((src :accessor gl-program-object-src
	:initarg :src)
   (handle :accessor gl-program-object-handle)
   (uniforms :accessor gl-program-object-uniforms)))

(defun use-gl-program (src)
  (gl:use-program (gl-program-object-handle src)))

(defun create-gl-program (src)
  (let ((inst
	 (make-instance 'gl-program-object :src src))
	(obj (glslgen::gl-dump-shader src)))
    (setf (gl-program-object-handle inst)
	  obj)
    (setf (gl-program-object-uniforms inst)
	  (cache-program-uniforms
	   obj
	   (glslgen::shader-program-data-raw-uniform src)))
    inst))
