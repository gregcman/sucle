;;;; package.lisp

(defpackage #:the-matrix-is-everywhere
  (:use #:cl))

(defpackage #:mat
  (:use #:cl)
  (:export
   #:normalize! #:fourbyfour #:get-lookat #:hypot
   #:mmul #:mmul!
   #:matrix-multiply
   #:onebyfour #:fourbyone
   #:cross #:subtract #:subtract! #:add #:add!
   #:scale  #:scale! #:to-flat #:scaling-matrix
   #:identity-matrix #:translation-matrix
   #:rotation-matrix
   #:fucking-projection-matrix #:projection-matrix
   #:relative-lookat #:absolute-lookat #:easy-lookat))
