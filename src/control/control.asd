(asdf:defsystem #:control
  :author "terminal625"
  :license "MIT"
  :description "keyboard input, like wasd, and terminal emulator codes"
  :depends-on (#:window
	       #:utility
	       #:character-modifier-bits)
  :components 
  ((:file "control"))) 








