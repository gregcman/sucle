(asdf:defsystem #:window-glfw2
  :description "glfw2 opengl context creation, windowing and input"
  :author "Gregorio Manabat"
  :maintainer "Gregorio Manabat"

  :depends-on (#:cl-glfw2
	       #:funland)

  :serial t
  :components  
  ((:file "package")
   (:file "window")))
