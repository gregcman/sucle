(asdf:defsystem #:bundler-quicklisp
  :depends-on (#:quicklisp
	       #:filesystem-util)
  :components 
  ((:file "bundler-quicklisp"))) 

