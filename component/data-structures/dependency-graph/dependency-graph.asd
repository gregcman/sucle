(asdf:defsystem #:dependency-graph
  :depends-on (#:utility
	       #:bordeaux-threads)
  :components
  ((:file "dependency-graph")))
