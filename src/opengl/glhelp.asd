(asdf:defsystem #:glhelp
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
