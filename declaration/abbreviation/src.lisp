(defpackage #:declaration-abbreviation
  (:use #:cl)
  (:nicknames #:decabb)
  (:export
   #:with-declaim-inline
   #:with-unsafe-speed))

(in-package :declaration-abbreviation)

(defmacro with-declaim-inline ((&rest names) &body body)
  `(progn
     (declaim (inline ,@names))
     ,@body
     (declaim (notinline ,@names))))

(defmacro with-unsafe-speed (&body body)
  `(locally (declare (optimize (speed 3) (safety 0)))
     ,@body))
