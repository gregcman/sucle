(in-package #:window)

(defparameter *keypress-hash* nil)
(defparameter *mousepress-hash* nil)

(defparameter *scroll-x* 0)
(defparameter *scroll-y* 0)

(defparameter *width* nil)
(defparameter *height* nil)

(progn
  (defmacro def-key-callback (name (key action) &body body)
    `(cffi:defcallback ,name :void
	 ((,key glfw::key) (,action :int))
       ,@body))

  (defmacro def-mouse-button-callback (name (button action) &body body)
    `(cffi:defcallback ,name :void
	 ((,button glfw::mouse)
	  (,action :int) )
       ,@body))
  (defmacro def-char-callback (name (char) &body body)
    `(cffi:defcallback ,name :void
	 ((,char :unsigned-int))
       ,@body)))

(defconstant +false+ 0)
(defconstant +release+ 1)
(defconstant +true+ 2)
(defconstant +press+ 3)
(defconstant +repeat+ 4)

(defconstant +mod-key-shift+ 3)
(defconstant +key-state-mask+ #b111)
(defconstant +mod-state-mask+ #b1111000)

(defparameter *action-map* (vector 1 3 4))

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

(def-key-callback key-callback (key action)
  (let ((vec *keys*))
    (flet ((add (x)
	     (vector-push-extend x vec)))
      (add key)
      (add action))))
(def-mouse-button-callback mouse-callback (button action)
  (let ((vec *buttons*))
    (flet ((add (x)
	     (vector-push-extend x vec)))
      (add button)
      (add action))))
(def-char-callback char-callback (char)
  (let ((vec *zchars*))
    (flet ((add (x)
	     (vector-push-extend x vec)))
      (add char))))

(defun make-fill-vector ()
  (make-array 0
	      :adjustable t
	      :fill-pointer 0))

(defparameter *buttons* (make-fill-vector))
(defparameter *keys* (make-fill-vector))
(defparameter *chars* (make-fill-vector))
(defparameter *zchars* (make-fill-vector))

(defparameter *last-y-scroll* 0)

(glfw:def-mouse-wheel-callback scroll-callback (y)
  (setf *scroll-y* (- y *last-y-scroll*))
  (setf *last-y-scroll* y))
(defparameter *status* nil)
(glfw:def-window-size-callback update-viewport (w h)
  (setf *width* w *height* h)
  (funcall *resize-hook* w h))
(glfw:def-window-close-callback close ()
  (setf *should-close-p* t)
  (setf *status* t)
  glfw:+false+)

(defparameter *resize-hook* (constantly nil))

(defun init ()
  (setf *last-y-scroll* 0)
  (setf *mouse-locked* :normal)
  (setf *scroll-x* 0
	*scroll-y* 0)
  (setf *should-close-p* nil)
  (if *keypress-hash*
      (clrhash *keypress-hash*)
      (setf *keypress-hash* (make-hash-table :test 'eq)))
  (if *mousepress-hash* 
      (clrhash *mousepress-hash*)
      (setf *mousepress-hash* (make-hash-table :test 'eq)))
  (setq *status* nil)
  #+sbcl (sb-int:set-floating-point-modes :traps nil))

(defparameter *should-close-p* nil)

(defun poll ()
  (setf *scroll-y* 0)
  (step-hash *keypress-hash*)
  (step-hash *mousepress-hash*)
  (setf (fill-pointer *zchars*) 0
	(fill-pointer *buttons*) 0
	(fill-pointer *keys*) 0)
  (setf (fill-pointer *chars*) 0)
  (glfw:poll-events)
  (setq *status* *should-close-p*)
  (let ((charsize (fill-pointer *zchars*)))
    (dobox ((offset 0 charsize :inc 1))
	   (etouq (with-vec-params '((offset char)) '(*zchars*)
		    '(if (< char char-code-limit)
		      (vector-push-extend (code-char char) *chars*))))))
  (let ((buttonsize (fill-pointer *buttons*)))
    (dobox ((offset 0 buttonsize :inc 2))     
	   (etouq (with-vec-params '((offset button action)) '(*buttons*)
		    '(let* (
			    (new-composite (aref *action-map* action)))
		      (declare (type fixnum  new-composite))
		      (setf (mice button) new-composite))))))
  (let ((keysize (fill-pointer *keys*)))
    (dobox ((offset 0 keysize :inc 2))
	   (etouq (with-vec-params '((offset key action)) '(*keys*)
		    '(let* (
			    (new-composite (aref *action-map* action)))
		      (declare (type fixnum new-composite))
		      (setf (key key) new-composite)))))))


(defun get-proc-address ()
  (function glfw:get-proc-address))

  ;;Graphics calls on OS X must occur in the main thread

(defun wrapper (func)
  (glfw:with-init
    (window:init)
    (glfw:with-open-window (:title "" :width 1 :height 1)
      (glfw:disable :auto-poll-events)
      (glfw:set-mouse-button-callback 'mouse-callback)
      (glfw:set-key-callback 'key-callback)
      (glfw:set-mouse-wheel-callback 'scroll-callback)
      (glfw:set-window-size-callback 'update-viewport)
      (glfw:set-char-callback 'char-callback)
      (glfw:set-window-close-callback 'close)
      (funcall func))))

(defun get-mouse-out ()
  (glfw:enable :mouse-cursor))

(defparameter *mouse-locked* :normal)

(defun toggle-mouse-capture ()
  (if (mice-locked-p)
      (progn
	(setf *mouse-locked* :disabled)
	(glfw:disable :mouse-cursor))
      (progn
	(setf *mouse-locked* :normal)
	(glfw:enable :mouse-cursor))))

(defun mice-locked-p ()
  (eq :disabled *mouse-locked*))

(defun mice-free-p ()
  (eq :normal *mouse-locked*))

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
  (glfw:get-mouse-pos))
