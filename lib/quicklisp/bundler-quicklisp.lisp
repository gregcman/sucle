(defpackage #:bundler-quicklisp
  (:use #:cl))
(in-package :bundler-quicklisp)

(defparameter *bundles-dir*
  (merge-pathnames
   "bundle/"
   (asdf:system-source-directory :bundler-quicklisp)))

(defparameter *bundle-file*
  (merge-pathnames
   "bundle.lisp"
   *bundles-dir*))

(defun bundle-systems ()
  (ql:bundle-systems *systems* :to *bundles-dir*))

(defparameter *systems*
  '("iterate"
    "alexandria"
    "closer-mop"
    "read-number"
    "cffi"
    "split-sequence"
    "opticl"
    "cl-openal"
    "cl-alc"
    "bordeaux-threads"
    "lparallel"
    "cl-opengl"
    "sb-cga"
    "defpackage-plus"
    "trivial-features"
    "cl-glfw3"
    ))

(defun load-bundle ()
  (load *bundle-file*))
