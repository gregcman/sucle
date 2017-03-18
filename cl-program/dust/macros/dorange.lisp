(defpackage #:dorange
  (:use #:cl)
  (:export #:dorange))

(in-package :dorange)

(defmacro dorange ((var times-form start-form) &rest body)
  (let ((times (gensym)))
    `(let ((,times ,times-form))
       (dobox ((,var ,times (+ ,times ,start-form))) ,@body))))
