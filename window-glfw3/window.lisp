(in-package #:window)

(defparameter *keypress-hash* nil)
(defparameter *mousepress-hash* nil)

(defparameter *scroll-x* nil)
(defparameter *scroll-y* nil)

(defparameter *width* nil)
(defparameter *height* nil)

(progn
  (defmacro def-key-callback (name (window key scancode action mod-keys) &body body)
    `(%glfw:define-glfw-callback ,name
	 ((,window :pointer) (,key :int) (,scancode :int)
	  (,action :int) (,mod-keys :unsigned-int))
       ,@body))

  (defmacro def-mouse-button-callback (name (window button action mod-keys) &body body)
    `(%glfw:define-glfw-callback ,name
	 ((,window :pointer) (,button :int)
	  (,action :int) (,mod-keys :unsigned-int))
       ,@body)))

;;;release = 0 press = 1 repeat = 2

(defconstant +release+ 0)
(defconstant +press+ 1)
(defconstant +repeat+ 2)

;;;when buttons can take either of two states, there are four
;;;ways adjacent time frames can look [repeat does not count here]
(defun next-key-state (old new)
  (cond ((eql new +release+)
	 (cond ((eq nil old) nil)
	       ((eql +press+ old) +release+)
	       ((eql +release+ old) nil)
	       (t +release+)))
	((eql new +press+)
	 (cond ((eq nil old) +press+)
	       ((eql +press+ old) t)
	       ((eql +release+ old) +press+)
	       (t t)))
	((eql new +repeat+)
	 (cond ((eq nil old) (print "wtf?") +press+)
	       ((eql +press+ old) t)
	       ((eql +release+ old) (print "huh?") +press+)
	       (t t)))))

(defun step-hash (hash)
  (with-hash-table-iterator (next hash)
    (loop (multiple-value-bind (more key value) (next)
	    (if more
		(cond ((eql value +press+) (setf (gethash key hash) t))
		      ((eql value +release+) (setf (gethash key hash) nil))
		      ((eq value nil) (remhash key hash)))
		(return))))))

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
      (or (eq value t)
	  (eql +press+ value))))
  (defun key-r (the-key)
    (let ((value (key the-key)))
      (or (eq nil value)
	  (eql value +release+))))
  (defun key-j-p (the-key)
    (let ((value (key the-key)))
      (eq value +press+)))
  (defun key-j-r (the-key)
    (let ((value (key the-key)))
      (eq value +release+)))

;;;for mice
  (defun mice-p (the-key)
    (let ((value (mice the-key)))
      (or (eq value t)
	  (eql value +press+))))
  (defun mice-r (the-key)
    (let ((value (mice the-key)))
      (or (eq value nil)
	  (eql value +release+))))
  (defun mice-j-p (the-key)
    (let ((value (mice the-key)))
      (eq value +press+)))
  (defun mice-j-r (the-key)
    (let ((value (mice the-key)))
      (eq value +release+)))

;;;glfw callbacks which will update the hashes to contain nil t
;;;+press+ or +release+ per key [each key is a symbol]
  (def-key-callback key-callback (window key scancode action mod-keys)
    (declare (ignorable scancode mod-keys window))
 ;   (print mod-keys)
    (let ((old-value (key key)))
      (setf (key key) (next-key-state old-value action))))
  (def-mouse-button-callback mouse-callback (window button action mod-keys)
    (declare (ignorable mod-keys window))
    (let ((old-value (mice button)))
      (setf (mice button) (next-key-state old-value action)))))


(glfw:def-scroll-callback scroll-callback (window x y)
  (declare (ignore window))
  (setf *scroll-x* (coerce x 'single-float)
	*scroll-y* (coerce y 'single-float)))
(defparameter *status* nil)
(glfw:def-window-size-callback update-viewport (window w h)
  (declare (ignorable window))
  (setf *width* w *height* h)
  (funcall *resize-hook* w h))

(defparameter *resize-hook* (constantly nil))

(defun init ()
  (setf *scroll-x* 0.0
	*scroll-y* 0.0)
  (if *keypress-hash*
      (clrhash *keypress-hash*)
      (setf *keypress-hash* (make-hash-table :test 'eq)))
  (if *mousepress-hash* 
      (clrhash *mousepress-hash*)
      (setf *mousepress-hash* (make-hash-table :test 'eq)))
  (setq *status* nil)
  #+sbcl (sb-int:set-floating-point-modes :traps nil))

(defun poll ()
  (setf *scroll-x* 0.0
	*scroll-y* 0.0)
  (setq *status* (glfw:window-should-close-p))
  (step-hash *keypress-hash*)
  (step-hash *mousepress-hash*)
  (glfw:poll-events))


(defun get-proc-address ()
  #'glfw:get-proc-address)

;;Graphics calls on OS X must occur in the main thread
(defun wrapper (func)
  (glfw:with-init
    (window:init)
    (glfw:with-window (:title "" :width 1 :height 1 :resizable nil)
      (glfw:set-mouse-button-callback 'mouse-callback)
      (glfw:set-key-callback 'key-callback)
      (glfw:set-scroll-callback 'scroll-callback)
      (glfw:set-window-size-callback 'update-viewport)
      (funcall func))))

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

(defun get-mouse-position (&optional (window glfw:*window*))
  (cffi:with-foreign-objects ((x :int) (y :int))
    (cffi:foreign-funcall "glfwGetCursorPos"
			  %glfw::window window :pointer x :pointer y :void)
    (values (coerce (cffi:mem-ref x :double) 'single-float)
	    (coerce (cffi:mem-ref y :double) 'single-float))))
