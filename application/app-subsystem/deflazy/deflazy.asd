(asdf:defsystem #:deflazy
  :depends-on (#:utility
	       #:bordeaux-threads)
  :components
  ((:file "dependency-graph")
   (:file "lazy-loading")))
