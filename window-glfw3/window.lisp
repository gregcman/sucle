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
	 ((,window :pointer) (,key %glfw::key) (,scancode :int)
	  (,action :int) (,mod-keys :unsigned-int))
       ,@body))

  (defmacro def-mouse-button-callback (name (window button action mod-keys) &body body)
    `(%glfw:define-glfw-callback ,name
	 ((,window :pointer) (,button %glfw::mouse)
	  (,action :int) (,mod-keys :unsigned-int))
       ,@body))
  (defmacro def-char-callback (name (window char) &body body)
    `(%glfw:define-glfw-callback ,name
	 ((,window :pointer) (,char :unsigned-int))
       ,@body)))

;;;release = 0 press = 1 repeat = 2

(defconstant +release+ 0)
(defconstant +press+ 1)
(defconstant +repeat+ 2)

(defconstant +true+ 3)

(defconstant +mod-key-shift+ 2)
(defconstant +key-state-mask+ 3)

;;;when buttons can take either of two states, there are four
;;;ways adjacent time frames can look [repeat does not count here]
(defun next-key-state (old new)
  (cond ((eq nil old)
	 (if (eql new +press+) +press+))
	((eq +true+ old)
	 (cond ((eql new +release+) +release+)
	       ((eql new +repeat+) +repeat+)))))

(defun step-hash (hash)
  (with-hash-table-iterator (next hash)
    (loop (multiple-value-bind (more key value) (next)
	    (if more
		(if value 
		    (let ((nvalue (logand +key-state-mask+ value)))
		      (cond ((eql nvalue +true+))
			    ((eql nvalue +press+) (setf (gethash key hash) +true+))
			    ((eql nvalue +release+) (setf (gethash key hash) nil))
			    ((eql nvalue +repeat+) (setf (gethash key hash) +true+))))
		    (remhash key hash))
		(return))))))

(macrolet ((key (key)
	     `(gethash ,key *keypress-hash*))
	   (mice (mice)
	     `(gethash ,mice *mousepress-hash*)))
;;;;various functions to test the state of the keyboard

;;;"-p" stands for "press" or predicate
;;;"-r" stands for "repeat"
;;;"-j" stands for "just" as in "it was not pressed just a moment ago,
;;;now it is"

;;;for the keyboard
  (defun key-p (the-key)
    (let ((value (key the-key)))
      (when value
	(let ((value (logand value +key-state-mask+)))
	  (or (eq value +true+)
	      (eql +press+ value)
	      (eql +repeat+ value))))))
  (defun key-r-or-p (the-key)
    (let ((value (key the-key)))
      (when value
	(let ((value (logand value +key-state-mask+)))
	  (or (eql +press+ value)
	      (eql +repeat+ value))))))
  (defun key-r (the-key)
    (let ((value (key the-key)))
      (when value
	(let ((value (logand value +key-state-mask+)))
	  (eql value +repeat+)))))
  (defun key-j-p (the-key)
    (let ((value (key the-key)))
      (when value
	(let ((value (logand value +key-state-mask+)))
	  (eq value +press+)))))
  (defun key-j-r (the-key)
    (let ((value (key the-key)))
      (when value
	(let ((value (logand value +key-state-mask+)))
	  (eq value +release+)))))

;;;for mice
  (defun mice-p (the-key)
    (let ((value (mice the-key)))
      (when value
	(let ((value (logand value +key-state-mask+)))
	  (or (eq value +true+)
	      (eql value +press+)
	      (eql value +repeat+))))))
  (defun mice-r-or-p (the-key)
    (let ((value (mice the-key)))
      (when value
	(let ((value (logand value +key-state-mask+)))
	  (or (eql value +press+)
	      (eql value +repeat+))))))
  (defun mice-r (the-key)
    (let ((value (mice the-key)))
      (when value
	(let ((value (logand value +key-state-mask+)))
	  (eql value +repeat+)))))
  (defun mice-j-p (the-key)
    (let ((value (mice the-key)))
      (when value
	(let ((value (logand value +key-state-mask+)))
	  (eq value +press+)))))
  (defun mice-j-r (the-key)
    (let ((value (mice the-key)))
      (when value
	(let ((value (logand value +key-state-mask+)))
	  (eq value +release+)))))

;;;glfw callbacks which will update the hashes to contain nil t
;;;+press+ or +release+ per key [each key is a symbol]
  (def-key-callback key-callback (window key scancode action mod-keys)
    (let ((vec *keys*))
      (flet ((add (x)
	       (vector-push-extend x vec)))
	(add window)
	(add key)
	(add scancode)
	(add action)
	(add mod-keys))))
  (def-mouse-button-callback mouse-callback (window button action mod-keys)
    (let ((vec *buttons*))
      (flet ((add (x)
	       (vector-push-extend x vec)))
	(add window)
	(add button)
	(add action)
	(add mod-keys))))
  (def-char-callback char-callback (window char)
    (let ((vec *zchars*))
      (flet ((add (x)
	       (vector-push-extend x vec)))
	(add window)
	(add char))))

  (defparameter *buttons* (make-fill-vector))
  (defparameter *keys* (make-fill-vector))
  (defparameter *chars* (make-fill-vector))
  (defparameter *zchars* (make-fill-vector))

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
    (setf (fill-pointer *zchars*) 0
	  (fill-pointer *buttons*) 0
	  (fill-pointer *keys*) 0)
    (setf (fill-pointer *chars*) 0)
    (glfw:poll-events)
    (let ((charsize (fill-pointer *zchars*)))
      (dobox ((offset 0 charsize :inc 2))
	     (with-vec-params (window char) (*zchars* offset)
	       (declare (ignorable window))
	       (vector-push-extend (code-char char) *chars*))))
    (let ((buttonsize (fill-pointer *buttons*)))
      (dobox ((offset 0 buttonsize :inc 4))     
	     (with-vec-params (window button action mod-keys) (*buttons* offset)
	       (declare (ignorable window))
	       (let* ((key-state (mice button))
		      (mod-shift (ash mod-keys +mod-key-shift+))
		      (new (next-key-state key-state action))
		      (new-composite (logior mod-shift new)))
		 (declare (type fixnum mod-shift new new-composite))
		 (setf (mice button) new-composite)))))
    (let ((keysize (fill-pointer *keys*)))
      (dobox ((offset 0 keysize :inc 5))
	     (with-vec-params (window key scancode action mod-keys) (*keys* offset)
	       (declare (ignorable window scancode))
	       (let* ((key-state (key key))
		      (mod-shift (ash mod-keys +mod-key-shift+))
		      (new (next-key-state key-state action))
		      (new-composite (logior mod-shift new)))
		 (declare (type fixnum mod-shift new new-composite))
		 (setf (key key) new-composite)))))))


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
      (glfw:set-char-callback 'char-callback)
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

(defun make-fill-vector ()
  (make-array 0
	      :adjustable t
	      :fill-pointer 0))
