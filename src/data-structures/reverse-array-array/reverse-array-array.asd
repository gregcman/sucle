(asdf:defsystem #:reverse-array-array
  :author "terminal625"
  :license "MIT"
  :description "a highly optimized data structure for fast writing and ok n(1) random access"
  :depends-on
  (#:reverse-array-iterator
   #:utility)
  :serial t
  :components    
  ((:file "reverse-array-array")
   (:file "iterator")))

