(asdf:defsystem #:opengl-glfw3
  :description "glfw3 opengl context creation, windowing and input"

  :depends-on (#:glfw-3-2
	       #:utility)

  :serial t
  :components  
  ((:file "package")
   (:file "enum")
   (:file "input-array")
   (:file "window")))
