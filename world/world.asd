(asdf:defsystem #:world
  :description "the minecraft world is a singleton. contains code to access blocks"
  :author "Gregorio Manabat"
  :maintainer "Gregorio Manabat"

  :depends-on (:vox :macrology :queue :pix :recycler)

  :serial t
  :components  
  ((:file "package")
   (:file "world")))
