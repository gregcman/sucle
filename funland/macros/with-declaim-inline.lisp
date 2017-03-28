(in-package :fuktard)
(export (quote with-declaim-inline))
(defmacro with-declaim-inline ((&rest names) &body body)
  `(progn
     (declaim (inline ,@names))
     ,@body
     (declaim (notinline ,@names))))
