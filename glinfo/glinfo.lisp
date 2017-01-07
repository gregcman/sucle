(in-package :glinfo)

(defun sizeof (type-keyword)
  "gets the size of a foreign c type"
  (cffi:foreign-type-size type-keyword))

(defun get-gl-constant (keyword)
  "gets a gl-constant"
  (cffi:foreign-enum-value '%gl:enum keyword))

