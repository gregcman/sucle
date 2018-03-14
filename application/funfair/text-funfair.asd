(asdf:defsystem #:text-funfair
  :depends-on (#:opticl
	       #:application
	       #:iterator
	       #:singleton-lparallel)
  :components
  ((:file "text-funfair")))
