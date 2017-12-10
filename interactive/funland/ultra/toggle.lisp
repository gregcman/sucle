(in-package funland)
(defmacro toggle (var) `(setf ,var (not ,var)))
