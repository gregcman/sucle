(in-package :sandbox)

(progno
 (defun sizeof (type-keyword)
   (cffi:foreign-type-size type-keyword))
 (defun get-gl-constant (keyword)
   (cffi:foreign-enum-value '%gl:enum keyword))
 )
