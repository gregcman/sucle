(defpackage #:sucle-temp
  (:use :cl)
  (:export
   #:path))
(in-package :sucle-temp)

(defun path (path) 
  (merge-pathnames
   path
   (merge-pathnames
    "files/"
    (asdf:system-source-directory :sucle-temp))))
