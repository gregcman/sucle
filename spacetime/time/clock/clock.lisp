(defpackage :clock
  (:use #:cl #:utility)
  (:export #:microseconds))
(in-package :clock)
(defparameter *seconds-offset* 1506020000)
(defun microseconds (&optional (offset *seconds-offset*))
  #+sbcl
  (multiple-value-bind (s m) (sb-ext:get-time-of-day)
    (+ (* (expt 10 6) (- s offset)) m))
  #-sbcl
  (* (get-internal-real-time)
     (etouq (round (/ (expt 10 6) internal-time-units-per-second)))))
