(in-package :utility)

(declaim (inline floatify))
(defun floatify (x)
  (coerce x 'single-float))

(declaim (inline byte/255))
(defun byte/255 (x)
  (/ x 255.0))

(declaim (inline clamp))
(defun clamp (min max x)
  (max min (min max x)))
