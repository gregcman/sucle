(asdf:defsystem #:window-glfw3
  :description "glfw3 opengl context creation, windowing and input"
  :author "Gregorio Manabat"
  :maintainer "Gregorio Manabat"

  :depends-on (#:cl-glfw3
	       #:trivial-main-thread)

  :serial t
  :components  
  ((:file "package")
   (:file "window")))
