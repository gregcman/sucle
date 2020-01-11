(asdf:defsystem #:scratch-buffer
  :author "terminal625"
  :license "MIT"
  :description "fast write to a buffer, like vector-push-extend. 
a highly optimized data structure for fast writing and ok n(1) random access,
highly optimized iterator for writing to the reverse-array-array"
  :depends-on (#:utility
	       #:bordeaux-threads)
  :components
  ( ;;iterator
   (:file "reverse-array-iterator")
   (:file "user")
   ;;array of arrays
   (:file "reverse-array-array")
   (:file "iterator")
   (:file "example")
   
   (:file "scratch-buffer")))
