;;;; cl-glfw3.asd

(asdf:defsystem #:cl-glfw3
  :serial t
  :description "Bindings for GLFW 3.x"
  :author "Alex Charlton <alex.n.charlton@gmail.com>"
  :license "BSD-2"
  :depends-on (#:cffi #:alexandria)
  :components ((:file "package")
               (:file "glfw-bindings")
               (:file "cl-glfw3")))
