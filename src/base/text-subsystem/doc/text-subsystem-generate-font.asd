(asdf:defsystem #:text-subsystem-generate-font
  :depends-on (#:utility
	       #:cl-freetype2
	       #:opticl)
  :components 
  ((:file "fonts")))

