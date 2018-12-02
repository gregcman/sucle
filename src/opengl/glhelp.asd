(asdf:defsystem #:glhelp
  :depends-on (#:cl-opengl
	       #:split-sequence)
  :serial t
  :components
  ((:file "glhelp")
   (:file "glslgen")
   (:file "glslgen2")
   (:file "handles")
   (:file "other")))
