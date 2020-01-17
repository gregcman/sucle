(asdf:defsystem #:glhelp
  :author "terminal625"
  :license "MIT"
  :description "Generate GLSL that runs on all versions but leaves out features that are backwards incompatible and All OpenGL code utilities I've written."
  :depends-on (#:cl-opengl
	       #:split-sequence ;;[FIXME]this is used in one place?
	       #:deflazy
	       #:uncommon-lisp
	       #:glsl-toolkit)
  :serial t
  :components
  ((:file "opengl-helper")))
