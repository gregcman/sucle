(in-package fuktard)
(defmacro toggle (var) `(setf ,var (not ,var)))
