(asdf:defsystem #:window
  :description "a windowing thingy"
  :version "0.0.0"
  :author "morpheus"
  :maintainer "tain mainer"
  :licence "i am not sure"

  :depends-on (#:cl-opengl
               #:lispbuilder-sdl)

  :serial t
  :components  
  ((:file "package")
   (:file "window")))
