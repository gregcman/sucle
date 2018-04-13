(asdf:defsystem #:color-test
  :depends-on (#:utility
	       #:rs-colors
	       #:opticl)
  :serial t
  :components 
  ((:file "color-test")))
