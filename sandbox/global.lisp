(in-package :sandbox)

;;;;in this file:: global state
;;;;naturally opengl objects will go here: opengl is a global state machine
;;;;call lists, textures, shader program, thread libraries cleared
;;;;on initilialization
;;;;threads are global
;;;;loaded images will go here

;;;;"g" stands for "global"
(defparameter *g/image* nil) ;;raw image arrays
(defparameter *g/text* nil);;text: sequences of bytes
(defparameter *g/tree* nil);;trees: code, directory structure...
(defparameter *g/thread* nil);; threads

;;;;opengl works with numbers
(defparameter *g/call-list* nil);;opengl call lists
(defparameter *g/texture* nil);;opengl textures
(defparameter *g/shader* nil);;opengl shaders

(defparameter *g/args* nil);;original arguments passed to main

(defmacro ensure-lib (libname type)
  `(unless ,libname
     (setf ,libname (create-lib ',type))))

;;equal is less time critical than eq and can handle strings
(defun ensure-all-libs ()
  (ensure-lib *g/image* equal)
  (ensure-lib *g/text* equal)
  (ensure-lib *g/tree* equal)

  (ensure-lib *g/args* eq)
  (ensure-lib *g/thread* eq)
  (ensure-lib *g/call-list* eq)
  (ensure-lib *g/shader* eq)
  (ensure-lib *g/texture* eq))

(defun clear-live-libs ()
  (lclear *g/args*)
  (lclear *g/thread*)
  (lclear *g/call-list*)
  (lclear *g/shader*)
  (lclear *g/texture*))

(defun (setf lget) (new lib name)
  (lset lib name new))

(defun create-lib (test)
  (make-hash-table :test test))
(defun lset (lib name obj)
  (setf (gethash name lib) obj))
(defun lget (lib name)
  (gethash name lib))
(defun lclear (lib)
  (clrhash lib))
(defun lremove (lib name)
  (remhash name lib))

(defun initlib ()
  (ensure-all-libs)
  (clear-live-libs))
