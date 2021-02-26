(asdf:defsystem #:sucle
  :author "terminal625"
  :license "MIT"
  :description "Cube Demo Game"
  :depends-on
  (
   #:sucle-base
   #:alexandria  
   #:utility

   #:sucle-base
   #:aabbcc ;;for occlusion culling 
   #:livesupport
   #:black-tie)
  :serial t
  :components 
  (
   (:file "queue")
   (:file "voxel-chunks")
   (:file "package")
   (:file "util")
   (:file "menu")
   (:file "menus")
   ;;(:file "block-light") ;;light propogation
   (:file "mesher")
   (:file "block-data")
   (:file "world")
   (:file "extra")
   (:file "physics")
   (:file "sucle")
   (:file "render")))
