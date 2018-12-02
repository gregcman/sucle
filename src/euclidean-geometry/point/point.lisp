(defpackage #:point
  (:use #:cl)
  (:export
   #:point
   #:x
   #:y
   #:point-x
   #:point-y))

(in-package #:point)
(defclass point ()
  ((x :accessor point-x
       :initform 0.0
       :initarg :x)
   (y :accessor point-y
       :initform 0.0
       :initarg :y))) 
