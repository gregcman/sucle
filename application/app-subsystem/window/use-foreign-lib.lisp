(defpackage #:window-glfw-load-shared-library
  (:use :cl))
(in-package #:window-glfw-load-shared-library)

(cffi:define-foreign-library (glfw1234)
  (:darwin "libglfw.dylib.bodged")
  (:unix "libglfw.so.bodged")
  (:windows "libglfw.dll.bodged"))

(cffi:use-foreign-library glfw1234)
