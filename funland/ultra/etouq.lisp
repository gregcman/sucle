(in-package :fuktard)

(export (quote etouq))
#+nil
(defmacro etouq (form)
  (eval form))

(defmacro etouq (form)
  (let ((var (gensym)))
    `(macrolet ((,var () ,form))
       (,var))))
