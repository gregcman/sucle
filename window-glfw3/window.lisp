(in-package #:window)

(defparameter *keypress-hash* nil)
(defparameter *mousepress-hash* nil)

(defparameter *scroll-x* nil)
(defparameter *scroll-y* nil)

(defparameter *width* nil)
(defparameter *height* nil)

;;;when buttons can take either of two states, there are four
;;;ways adjacent time frames can look [repeat does not count here]
(defun next-key-state (old new)
  (case new
    (:press (case old
	      ((nil) :just-pressed)
	      (:just-pressed t)
	      (:just-released :just-pressed)
	      ((t) t)))
    (:release (case old
		((nil) nil)
		(:just-pressed :just-released)
		(:just-released nil)
		((t) :just-released)))
    (:repeat (case old
	       ((nil) (print "wtf?") :just-pressed)
	       (:just-pressed t)
	       (:just-released (print "huh?") :just-pressed)
	       ((t) t)))))

(defun step-hash (hash)
  (loop for key being the hash-keys of hash
     using (hash-value value)
     do (case value
	  (:just-pressed (setf (gethash key hash) t))
	  (:just-released (setf (gethash key hash) nil))
	  ((nil) (remhash key hash)))))

(macrolet ((key (key)
	     `(gethash ,key *keypress-hash*))
	   (mice (mice)
	     `(gethash ,mice *mousepress-hash*)))
;;;;various functions to test the state of the keyboard

;;;"-p" stands for "press" or predicate
;;;"-r" stands for "release"
;;;"-j" stands for "just" as in "it was not pressed just a moment ago,
;;;now it is"

;;;for the keyboard
  (defun key-p (the-key)
    (let ((value (key the-key)))
      (case value
	((t :just-pressed) t))))
  (defun key-r (the-key)
    (let ((value (key the-key)))
      (case value
	((nil :just-released) t))))
  (defun key-j-p (the-key)
    (let ((value (key the-key)))
      (eq value :just-pressed)))
  (defun key-j-r (the-key)
    (let ((value (key the-key)))
      (eq value :just-released)))

;;;for mice
  (defun mice-p (the-key)
    (let ((value (mice the-key)))
      (case value
	((t :just-pressed) t))))
  (defun mice-r (the-key)
    (let ((value (mice the-key)))
      (case value
	((nil :just-released) t))))
  (defun mice-j-p (the-key)
    (let ((value (mice the-key)))
      (eq value :just-pressed)))
  (defun mice-j-r (the-key)
    (let ((value (mice the-key)))
      (eq value :just-released)))

;;;glfw callbacks which will update the hashes to contain nil t
;;;:just-pressed or :just-released per key [each key is a symbol]
  (glfw:def-key-callback key-callback (window key scancode action mod-keys)
    (declare (ignorable scancode mod-keys window))
    (let ((old-value (key key)))
      (setf (key key) (next-key-state old-value action))))
  (glfw:def-mouse-button-callback mouse-callback (window button action mod-keys)
    (declare (ignorable mod-keys window))
    (let ((old-value (mice button)))
      (setf (mice button) (next-key-state old-value action)))))


(glfw:def-scroll-callback scroll-callback (window x y)
  (declare (ignore window))
  (setf *scroll-x* x
	*scroll-y* y))
(defparameter *status* nil)
(glfw:def-window-size-callback update-viewport (window w h)
  (setf *width* w *height* h)
  (funcall *resize-hook* w h))

(defparameter *resize-hook* (constantly nil))

(defun init ()
  (setf *scroll-x* 0.0d0
	*scroll-y* 0.0d0)
  (if *keypress-hash*
      (clrhash *keypress-hash*)
      (setf *keypress-hash* (make-hash-table :test 'eq)))
  (if *mousepress-hash* 
      (clrhash *mousepress-hash*)
      (setf *mousepress-hash* (make-hash-table :test 'eq)))
  (setq status nil)
  (setq mousecapturestate nil)
  (sb-int:set-floating-point-modes :traps nil))

(defun poll ()
  (setf *scroll-x* 0.0d0
	*scroll-y* 0.0d0)
  (setq *status* (glfw:window-should-close-p))
  (step-hash *keypress-hash*)
  (step-hash *mousepress-hash*)
  (glfw:poll-events))

(defun opengl-main-thread-p ()
  (or
   #+darwin t
   ))

(defun get-proc-address ()
  #'glfw:get-proc-address)

;;Graphics calls on OS X must occur in the main thread
(defun wrapper (func)
  (flet ((wrap ()
	   (glfw:with-init-window (:title ""
				   :width 1 :height 1
				   :resizable t)
	     (glfw:set-mouse-button-callback 'mouse-callback)
	     (glfw:set-key-callback 'key-callback)
	     (glfw:set-scroll-callback 'scroll-callback)
	     (glfw:set-window-size-callback 'update-viewport)
	     (funcall func))))
    (if (opengl-main-thread-p)
	(trivial-main-thread:with-body-in-main-thread () (wrap))
	(wrap))))

(defun get-mouse-out ()
  (glfw:set-input-mode :cursor :normal))

(defun toggle-mouse-capture ()
  (if (mice-locked-p)
      (glfw:set-input-mode :cursor :normal)
      (glfw:set-input-mode :cursor :disabled)))

(defun mice-locked-p ()
  (eq :disabled (glfw:get-input-mode :cursor)))

(defun mice-free-p ()
  (eq :normal (glfw:get-input-mode :cursor)))

(defun push-dimensions (width height)
  (setf *width* width
	*height* height)
  (glfw:set-window-size width height))

(defun set-caption (caption)
  (glfw:set-window-title caption))

(defun update-display ()
  (glfw:swap-buffers))

(defun set-vsync (bool)
  (if bool
      (glfw:swap-interval 1) ;;1 is on
      (glfw:swap-interval 0))) ;;0 is off

(defun get-mouse-position ()
  (values-list (glfw:get-cursor-position)))
