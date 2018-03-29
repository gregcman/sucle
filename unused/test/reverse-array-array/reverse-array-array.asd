(asdf:defsystem #:reverse-array-array
  :depends-on
  (#:iterator
   #:utility)
  :serial t
  :components    
  ((:file "reverse-array-array")
   (:file "iterator")))

