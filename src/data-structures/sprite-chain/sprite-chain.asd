(asdf:defsystem #:sprite-chain
  :author "terminal625"
  :license "MIT"
  :description "A data structure for sprites like how it works in the Scratch programming system"
  :depends-on (#:sucle-doubly-linked-list
	       #:uncommon-lisp)
  :serial t
  :components 
  ((:file "sprite-chain")))
