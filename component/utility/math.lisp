(in-package :utility)

(defmacro floatify (x)
  `(coerce ,x 'single-float))
