(in-package :sandbox)

(defun clamp (x min max)
  (max (min x max) min))
(defun rad-deg (rad)
  "converts radians to degrees"
  (* rad 180 (/ 1 pi)))
(defun deg-rad (deg)
  "converts degrees to radians"
  (* deg pi 1/180))

(defun ease (x target fraction)
  (+ x (* fraction (- target x))))

(define-modify-macro *= (&rest args)
  *)

(defun int-scale (int scale)
  (truncate (* int scale)))
