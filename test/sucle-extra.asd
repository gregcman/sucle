(asdf:defsystem #:sucle-extra
  :author "terminal625"
  :license "MIT"
  :description "Extra world drawing code"
  :depends-on
  (#:sucle
   #:black-tie
   #:cl-mathstats)
  :serial t
  :components 
  ((:file "extra")))
