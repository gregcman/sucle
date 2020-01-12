(asdf:defsystem #:text-subsystem
  :author "terminal625"
  :license "MIT"
  :description "Draw a grid of characters with bold or underline really fast in OpenGL"
  :depends-on
  (#:deflazy
   #:utility
   #:image-utility
   #:quads
   #:application
   #:sucle-temp
   #:sb-cga)
  :serial t
  :components 
  ((:file "text-subsystem"))) 
