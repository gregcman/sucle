(asdf:defsystem #:glhelp
  :depends-on (#:cl-opengl)
  :serial t
  :components
  ((:file "glhelp")
   (:file "glslgen")
   (:file "glslgen2")
   (:file "cache")
   (:file "other")))
