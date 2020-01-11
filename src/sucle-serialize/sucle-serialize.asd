(asdf:defsystem #:sucle-serialize
  :author "terminal625"
  :license "MIT"
  :description "data serialization"
  :depends-on (

	       ;;for serialize
	       #:cl-conspack
	       #:salza2
	       #:chipz

	       )
  :serial t
  :components 
  (
   (:file "serialize")
   ))
