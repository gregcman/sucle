(asdf:defsystem #:perez-sky-model
  :depends-on (#:utility
	       #:rs-colors
	       #:opticl
	       #:bad-floats
	       #:matrix)
  :components 
  ((:file "perez-sky-model")))
