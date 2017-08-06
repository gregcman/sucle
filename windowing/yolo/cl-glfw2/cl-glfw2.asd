(asdf:defsystem #:cl-glfw2
  :depends-on (cffi)
  :components ((:module lib :serial t 
                        :components ((:file "types")
				     (:file "package")
                                     (:file "cl-glfw2")
				     (:file "callbacks")))))
