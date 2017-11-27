(asdf:defsystem #:terminal625-glfw3
  :description "glfw3 opengl context creation, windowing and input"

  :depends-on (#:cl-glfw3
	       #:funland)

  :serial t
  :components  
  ((:file "package")
   (:file "window")))
