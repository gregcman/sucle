(asdf:defsystem #:glhelp
  :depends-on (#:cl-opengl)
  :serial t
  :components
  ((:file "glhelp")
   (:file "cache")
   (:file "other")))
