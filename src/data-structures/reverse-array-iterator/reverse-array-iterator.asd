(asdf:defsystem #:reverse-array-iterator
  :author "terminal625"
  :license "MIT"
  :description "highly optimized iterator for writing to the reverse-array-array"
  :depends-on (#:utility)
  :serial t
  :components
  ((:file "reverse-array-iterator")
   (:file "user")))
