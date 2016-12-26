(in-package :sandbox)
(defun sizeof (type-keyword)
  "gets the size of a foreign c type"
  (cffi:foreign-type-size type-keyword))

(defun get-gl-constant (keyword)
  "gets a gl-constant"
  (cffi:foreign-enum-value '%gl:enum keyword))

(defun glActiveTexture (num)
  "sets the active texture"
  (gl:active-texture (+ num (get-gl-constant :texture0))))
