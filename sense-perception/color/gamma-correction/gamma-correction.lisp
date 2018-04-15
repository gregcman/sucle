(defpackage :gamma-correction
  (:use #:cl)
  (:export
   #:gamma-correct))
(in-package :gamma-correction)

(defun gamma-correct (x &optional (gamma-value 2.33))
  (expt x gamma-value))
