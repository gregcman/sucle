(defpackage #:lem-sucle
  (:use :cl))
(in-package :lem-sucle)

(setf lem-sucle::*run-sucle* t)
(lem-paredit-mode:paredit-mode)

(dotimes (x 100)
  (print x))

(defparameter *an-overlay*
  (our-make-overlay 
   (lem:current-point)
   (lem:save-excursion
     (lem:forward-char 100)
     (lem:copy-point (lem:current-point) :temporary))
   (copy-attribute-to-sucle-attribute 'lem:cursor)))

(our-delete-overlay *an-overlay*)

;;data is stored in the overlay plist?


(delete-all-overlays)


(foo 1 2 3)
 hello world?