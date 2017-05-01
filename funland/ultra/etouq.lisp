(in-package :fuktard)

(export (quote etouq))
(defmacro etouq (form)
  (eval form))
