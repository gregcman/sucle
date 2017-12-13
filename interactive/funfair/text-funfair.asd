(asdf:defsystem #:text-funfair
  :depends-on (#:opticl
	       #:funfair
	       #:terminal625-zeorp
	       #:cl-program
	       #:glhelp)
  :serial t
  :components
  ((:file "test2")))
