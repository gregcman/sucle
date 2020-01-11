(asdf:defsystem #:scratch-buffer
  :author "terminal625"
  :license "MIT"
  :description "fast write to a buffer, like vector-push-extend"
  :depends-on (#:utility
	       #:reverse-array
	       #:bordeaux-threads)
    :components
    ((:file "scratch-buffer")))
