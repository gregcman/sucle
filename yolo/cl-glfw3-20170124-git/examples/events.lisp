;;;; events.lisp
;;;; This example shows events in the title bar.
(in-package #:cl-glfw3-examples)

(export '(events-example))

(defparameter *keys-pressed* nil)
(defparameter *buttons-pressed* nil)
(defparameter *window-size* nil)

(defun update-window-title (window)
  (set-window-title (format nil "size ~A | keys ~A | buttons ~A"
                            *window-size*
                            *keys-pressed*
                            *buttons-pressed*)
                    window))

(def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close))
  (if (eq action :press)
    (pushnew key *keys-pressed*)
    (deletef *keys-pressed* key))
  (update-window-title window))

(def-mouse-button-callback mouse-callback (window button action mod-keys)
  (declare (ignore mod-keys))
  (if (eq action :press)
    (pushnew button *buttons-pressed*)
    (deletef *buttons-pressed* button))
  (update-window-title window))

(def-window-size-callback window-size-callback (window w h)
  (setf *window-size* (list w h))
  (update-window-title window))

(defun events-example ()
  ;; Graphics calls on OS X must occur in the main thread
  (with-body-in-main-thread ()
    (with-init-window (:title "" :width 400 :height 400)
      (set-key-callback 'key-callback)
      (set-mouse-button-callback 'mouse-callback)
      (set-window-size-callback 'window-size-callback)
      (setf *window-size* (get-window-size))
      (update-window-title *window*)
      (loop until (window-should-close-p) do (wait-events)))))
