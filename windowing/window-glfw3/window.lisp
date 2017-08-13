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

(defconstant +false+ 0)
(defconstant +release+ 1)
(defconstant +true+ 2)
(defconstant +press+ 3)
(defconstant +repeat+ 4)

(defparameter *action-map* (vector 1 3 4))

(defconstant +mod-key-shift+ 3)
(defconstant +key-state-mask+ #b111)
(defconstant +mod-state-mask+ #b1111000)

(defun step-hash (hash)
  (with-hash-table-iterator (next hash)
    (loop (multiple-value-bind (more key value) (next)
	    (if more
		(let ((nvalue (get-press-value value)))
		  (cond ((= +true+ nvalue))
			((< +true+ nvalue)
			 (setf (gethash key hash)
			       (logior (get-mod-value value)
				       +true+)))
			((> +true+ nvalue)
			 (remhash key hash))))
		(return))))))

(progn
  (defun key (key)
    (gethash key *keypress-hash* +false+))
  (defun (setf key) (value key)
    (setf (gethash key *keypress-hash*) value))
  (defun mice (mice)
    (gethash mice *mousepress-hash* +false+))
  (defun (setf mice) (value mice)
    (setf (gethash mice *mousepress-hash*) value)))
;;;;various functions to test the state of the keyboard

;;;"-p" stands for "press" or predicate
;;;"-r" stands for "repeat"
;;;"-j" stands for "just" as in "it was not pressed just a moment ago,
;;;now it is"

;;;for the keyboard

(defun get-press-value (value)
  (logand value +key-state-mask+))
(defun get-mod-value (value)
  (logand value +mod-state-mask+))

(defun key-p (the-key)
  (let ((value (get-press-value (key the-key))))     
    (<= +true+ value)))
(defun key-r-or-p (the-key)
  (let ((value (get-press-value (key the-key))))     
    (< +true+ value)))
(defun key-r (the-key)
  (let ((value (get-press-value (key the-key))))     
    (= value +repeat+)))
(defun key-j-p (the-key)
  (let ((value (get-press-value (key the-key))))     
    (= value +press+)))
(defun key-j-r (the-key)
  (let ((value (get-press-value (key the-key))))     
    (= value +release+)))


(defun r-or-p (value)
  (< +true+ value))
;;;for mice


(defun mice-p (the-key)
  (let ((value (get-press-value (mice the-key))))
    (<= +true+ value)))
(defun mice-r-or-p (the-key)
  (let ((value (get-press-value (mice the-key))))
    (< +true+ value)))
(defun mice-r (the-key)
  (let ((value (get-press-value (mice the-key))))
    (= value +repeat+)))
(defun mice-j-p (the-key)
  (let ((value (get-press-value (mice the-key))))
    (= value +press+)))
(defun mice-j-r (the-key)
  (let ((value (get-press-value (mice the-key))))
    (= value +release+)))

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

(defun make-fill-vector ()
  (make-array 0
	      :adjustable t
	      :fill-pointer 0))

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
  (glfw:poll-events)
  (let ((charsize (fill-pointer *zchars*)))
    (dobox ((offset 0 charsize :inc 2))
	   (etouq (with-vec-params '((offset nil char)) '(*zchars*)
					;    (declare (ignorable window))
		    '(if (< char char-code-limit)
		      (vector-push-extend (code-char char) *chars*))))))
  (let ((buttonsize (fill-pointer *buttons*)))
    (dobox ((offset 0 buttonsize :inc 4))     
	   (etouq (with-vec-params '((offset nil button action mod-keys)) '(*buttons*)
					;    (declare (ignorable window))
		    '(let* ((mod-shift (ash mod-keys +mod-key-shift+))
			    (new-composite (logior mod-shift (aref *action-map* action))))
		      (declare (type fixnum mod-shift  new-composite))
		      (setf (mice button) new-composite))))))
  (let ((keysize (fill-pointer *keys*)))
    (dobox ((offset 0 keysize :inc 5))
	   (etouq (with-vec-params '((offset nil key nil action mod-keys)) '(*keys*)
					;  (declare (ignorable window scancode))
		    '(let* ((mod-shift (ash mod-keys +mod-key-shift+))
			    (new-composite (logior mod-shift (aref *action-map* action))))
		      (declare (type fixnum mod-shift new-composite))
		      (setf (key key) new-composite))))))
  (progn
    (setf (fill-pointer *zchars*) 0
	  (fill-pointer *buttons*) 0
	  (fill-pointer *keys*) 0)
    (setf (fill-pointer *chars*) 0)))


(defun get-proc-address ()
  (function glfw:get-proc-address))

;;Graphics calls on OS X must occur in the main thread

(defparameter *iresizable* nil)
(defparameter *iwidth* 1)
(defparameter *iheight* 1)
(defparameter *ititle* "Common Lisp")

(defun wrapper (func)
  (glfw:with-init
    (window:init)
    (glfw:with-window (:title *ititle*
			      :width *iwidth*
			      :height *iheight*
			      :resizable *iresizable*)
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
