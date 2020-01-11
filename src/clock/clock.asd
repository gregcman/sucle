(asdf:defsystem #:clock
  :author "terminal625"
  :license "MIT"
  :description "a millisecond or microsecond timer"
  :depends-on (#:local-time)
  :components 
  ((:file "clock")))
