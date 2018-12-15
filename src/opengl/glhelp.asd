(asdf:defsystem #:glhelp
  :author "terminal625"
  :license "MIT"
  :description "Generate GLSL that runs on all versions but leaves out features that are backwards incompatible and All OpenGL code utilities I've written."
  :depends-on (#:cl-opengl
	       #:split-sequence
	       #:deflazy
	       #:uncommon-lisp)
  :serial t
  :components
  ((:file "glhelp")
   (:file "glslgen")
   (:file "glslgen2")
   (:file "handles")
   (:file "other")))
