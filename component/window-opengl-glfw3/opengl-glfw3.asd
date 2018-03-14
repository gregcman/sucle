(asdf:defsystem #:opengl-glfw3
  :description "glfw3 opengl context creation, windowing and input"

  :depends-on (#:cl-glfw3
	       #:utility)

  :serial t
  :components  
  ((:file "package")
   (:file "window")))
