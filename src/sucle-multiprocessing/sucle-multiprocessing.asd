(asdf:defsystem #:sucle-multiprocessing
  :author "terminal625"
  :license "MIT"
  :description "multiprocessing fcailities"
  :depends-on (
	       ;;for multiprocessing
	       #:lparallel
	       #:bordeaux-threads
	       #:cl-cpus
	       #:uncommon-lisp
	       #:utility
	       )
  :serial t
  :components 
  (
   (:file "multiprocessing")
   ))
