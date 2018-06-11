(asdf:defsystem #:llvm-stuff
  :depends-on (#:cffi
	       #:utility)
  :components 
  ((:file "llvm-stuff")))
