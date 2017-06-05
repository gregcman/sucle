;;;; package.lisp

(defpackage #:cl-glfw3
  (:nicknames :glfw)
  (:use #:cl))

(defpackage #:%cl-glfw3
  (:nicknames :%glfw)
  (:use #:cl #:cffi #:alexandria))
