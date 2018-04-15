(defpackage :bad-floats
  (:use
   #:cl
   #:utility)
  (:export
   #:float-not-nan-p
   #:float-good-p
   #:with-float-traps-masked))

(in-package :bad-floats)

;;;nans are not equal to themselves
;;;nans are made by any operation with a nan
;;;and also: inf - inf, - inf + inf, 0 * inf, 0 / 0, inf / inf
(defmacro float-not-nan-p (x)
  (once-only (x)
    `(= ,x ,x)))

;;;multiplying infinity or nan by 0 will give nan
(defmacro float-good-p (x)
  (once-only (x)
    `(float-not-nan-p (* 0.0 ,x))))

(defmacro with-float-traps-masked (&body body)
  `(#+sbcl
    ,@'(sb-int:with-float-traps-masked (:invalid :overflow :inexact :underflow :divide-by-zero))
    #-sbcl
    progn
     ,@body))
