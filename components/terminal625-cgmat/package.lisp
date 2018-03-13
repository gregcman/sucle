(in-package :defpackage+-user-1)

(defpackage+ #:cg-matrix
  (:use #:cl)
  (:nicknames #:gcg)
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
