(asdf:defsystem #:reverse-array-array
  :depends-on
  (#:reverse-array-iterator
   #:utility)
  :serial t
  :components    
  ((:file "reverse-array-array")
   (:file "iterator")))

