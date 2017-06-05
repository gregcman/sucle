;;;; cl-glfw3-examples.asd

(asdf:defsystem #:cl-glfw3-examples
  :serial t
  :description "Examples for cl-glfw3"
  :author "Alex Charlton <alex.n.charlton@gmail.com>"
  :license "BSD-2"
  :depends-on (#:cl-glfw3 #:cl-opengl #:trivial-main-thread)
  :pathname "examples/"
  :components ((:file "package")
               (:file "basic-window")
               (:file "events")))
