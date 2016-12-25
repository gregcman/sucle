(asdf:defsystem #:vox
  :description "pretty big three dimensional voxel data, only valid up to +/- 2^23 or something"
  :version "0.0.0"
  :author "morpheus"
  :maintainer "tain mainer"
  :licence "i am not sure"

  :depends-on (
:macrology
:recycler		)

  :serial t
  :components  
  ((:file "package")
   (:file "vox")))
