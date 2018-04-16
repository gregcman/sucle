(defpackage :application-example-hello-world
  (:use #:cl)
  (:export #:start))
(in-package :application-example-hello-world)

(defun start ()
  (application:main
   (lambda ()
     (loop (application:poll-app)))
   :width 512
   :height 512
   :title "Hello World -_-")) 
