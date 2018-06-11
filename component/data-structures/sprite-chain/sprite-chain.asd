(asdf:defsystem #:sprite-chain
  :depends-on (#:doubly-linked-list
	       #:uncommon-lisp)
  :serial t
  :components 
  ((:file "sprite-chain")))
