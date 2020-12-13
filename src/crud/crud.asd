(asdf:defsystem #:crud
  :author "gregcman"
  :license "MIT"
  :description "Create, Read, Update, Delete. Key/value Persistent storage for lisp objects."
  :depends-on
  (#:sucle-serialize
   #:sqlite
   #:base64
   #:uiop
   #:lparallel
   ;;For one macro in 'database'
   #:alexandria
   #:log4cl)
  :serial t
  :components
  ((:file "database")
   (:file "crud"))) 








