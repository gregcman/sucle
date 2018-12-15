(asdf:defsystem #:deflazy
  :author "terminal625"
  :license "MIT"
  :description "lazy loading reloading on changes"
  :depends-on (#:utility
	       #:bordeaux-threads)
  :components
  ((:file "dependency-graph")
   (:file "lazy-loading")))
