(in-package :defpackage+-user-1)

(defpackage+ #:nsb-cga
  (:use #:cl)
  (:inherit #:sb-cga)
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
