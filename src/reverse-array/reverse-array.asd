(asdf:defsystem #:reverse-array
  :author "terminal625"
  :license "MIT"
  :description "a highly optimized data structure for fast writing and ok n(1) random access,
highly optimized iterator for writing to the reverse-array-array"
  :depends-on
  (#:utility)
  :serial t
  :components    
  (
   ;;iterator
   (:file "reverse-array-iterator")
   (:file "user")
   ;;array of arrays
   (:file "reverse-array-array")
   (:file "iterator")))

