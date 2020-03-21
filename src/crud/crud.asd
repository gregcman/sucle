(asdf:defsystem #:crud
  :author "gregcman"
  :license "MIT"
  :description "Create, Read, Update, Delete. Key/value Persistent storage for lisp objects."
  :depends-on
  (#:sucle-serialize
   #:sqlite
   #:cl-base64
   #:uiop
   #:lparallel
   #:sucle-multiprocessing
   ;;For one macro in 'database'
   #:alexandria)
  :serial t
  :components
  ((:file "database")
   (:file "crud"))) 








