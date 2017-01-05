(asdf:defsystem #:imagewise
  :description "image manipulation"
  :author "Gregorio Manabat"
  :maintainer "Gregorio Manabat"

  :depends-on (#:opticl)

  :serial t
  :components  
  ((:file "package")
   (:file "imagewise")))
