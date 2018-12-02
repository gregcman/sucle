(defpackage #:nsb-cga
  (:use #:cl)
  (:use #:sb-cga)
  (:export
   #:%matrix
   #:%identity-matrix
   #:%zero-matrix
   #:%reorient
   #:%rotate-around
   #:%rotate-around*
   #:%rotate*
   #:%rotate
   #:%scale*
   #:%scale
   #:%translate*
   #:%translate
   #:%matrix*
   #:%inverse-matrix
   #:%transpose-matrix))

(in-package :nsb-cga)
(cl-reexport:reexport-from :sb-cga)
