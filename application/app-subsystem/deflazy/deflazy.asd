(asdf:defsystem #:deflazy
  :depends-on (#:utility
	       #:bordeaux-threads
	       #:glhelp)
  :components
  ((:file "dependency-graph")
   (:file "lazy-loading")))
