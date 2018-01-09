(asdf:defsystem #:text-funfair
  :depends-on (#:opticl
	       #:funfair
	       #:terminal625-zeorp
	       #:cl-program
	       #:glhelp
	       #:singleton-lparallel)
  :serial t
  :components
  ((:file "text-funfair")))
