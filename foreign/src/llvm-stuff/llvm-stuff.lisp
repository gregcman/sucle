(defpackage :llvm-stuff
  (:use
   #:cl
   #:utility))

(in-package :llvm-stuff)

(pushnew "/home/imac/install/lib/" cffi::*foreign-library-directories* :test #'equal)

(progn
  (cffi:define-foreign-library libllvm
    (t (:default "libLLVM-7.0svn")))
  (cffi:use-foreign-library libllvm))
