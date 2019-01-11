(asdf:defsystem #:load-correct-glfw-library
  :author "terminal625"
  :license "MIT"
  :description "load correct glfw library"
  :depends-on ("cffi")
  :components ((:file "use-foreign-lib"))) 
