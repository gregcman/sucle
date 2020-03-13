(asdf:defsystem #:deflazy
  :author "terminal625"
  :license "MIT"
  :description "lazy loading reloading on changes"
  :depends-on (#:utility
	       #:bordeaux-threads
	       #:uncommon-lisp)
  :components
  ((:file "dependency-graph")
   (:file "lazy-loading")))
