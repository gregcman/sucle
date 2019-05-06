(defpackage :struct-to-clos
  (:use #:cl)
  (:export #:struct->class))

(in-package :struct-to-clos)

(defmacro struct->class ((defstruct name &body slots))
  (assert (eq 'cl:defstruct defstruct))
  `(structy-defclass:deftclass ,name ,@slots))
