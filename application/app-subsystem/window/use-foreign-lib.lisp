(defpackage #:window-glfw-load-shared-library
  (:use :cl))
(in-package #:window-glfw-load-shared-library)

(cffi:define-foreign-library (glfw)
  (:darwin
   (:or
    ;; homebrew naming
    "libglfw3.1.dylib" "libglfw3.dylib"
    ;; cmake build naming
    "libglfw.3.1.dylib" "libglfw.3.dylib"
    ;; bodge
    "libglfw.dylib.bodged"))
  (:unix
   (:or "libglfw.so.3.1" "libglfw.so.3"
	;;bodge
	"libglfw.so.bodged"))
  (:windows
   (:or "glfw3.dll"
	;;bodge
	"libglfw.dll.bodged"))
  (t (:or (:default "libglfw3")
	  (:default "libglfw"))))

(cffi:use-foreign-library glfw)
