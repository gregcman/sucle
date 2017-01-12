(in-package #:window)

;;only one window is allowed in sdl. But why would anyone want
;;more than tha?
(defparameter window nil) 
(defparameter status nil)
(defparameter wrapper nil)
(defparameter base-needs nil)

(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (case action
    (:press
     (key-down (pairlis
		      '(:state :scancode :key :mod :unicode)
		      (list nil scancode key nil nil))))
    (:release
     (key-up (pairlis
	      '(:state :scancode :key :mod :unicode)
	      (list nil scancode key nil nil)))))
  (in::update))

(glfw:def-mouse-button-callback mouse-callback (window button action mod-keys)
  (case action
    (:press
     (in::buttonshit button action nil nil))
    (:release
     (in::buttonshit button action nil nil)))
  (in::update))

(glfw:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (set-viewport w h))

(defun set-viewport (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -50 50 -50 50 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun get-gl-constant (keyword)
  "gets a gl-constant"
  (cffi:foreign-enum-value '%gl:enum keyword))

(defun arise ()
  (setq status nil)
  (setq window nil)
  (setq mousecapturestate nil)
  (sb-int:set-floating-point-modes :traps nil)
  (setq base-needs
	(lambda ()
	  (setq status (glfw:window-should-close-p))
	  (glfw:poll-events)
	  (more-window-shit)))
  (setq wrapper
	(lambda (func)
	  ;; Graphics calls on OS X must occur in the main thread
	  (trivial-main-thread:with-body-in-main-thread ()
	    (glfw:with-init-window (:title "proto" :width 16 :height 16
					   :opengl-profile :opengl-any-profile
					;;   :opengl-forward-compat (get-gl-constant :true)
					;;   :context-version-minor 2
					;;   :context-version-major 3
					   )
	      (out::init)
	      (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)
	      (glfw:set-mouse-button-callback 'mouse-callback)
	      (glfw:set-key-callback 'key-callback)
	      (glfw:set-window-size-callback 'update-viewport)
	      (unwind-protect
		   (if nil
		       (funcall func)
		       (wowwow func))
		(out::dead)))))))

(defparameter mousecapturestate nil)

(defun wowwow (func)
  (tagbody
     (handler-bind
	 ((error
	   (lambda (condition)
	     (print condition)
	     (get-mouse-out)
	     (restart-case
		 (let ((r (find-restart 'my-restart)))
		   (invoke-restart r))
	       (my-restart () (go huh))))))
       (funcall func))
     (return-from wowwow)
   huh
     (print "ENTER ANY NUMBER TO CONTINUE")
     (let ((response (read)))
       (if (numberp response)
	   (wowwow func)))))

(defun get-mouse-out ()
 (glfw:set-input-mode :cursor :normal))

(defun more-window-shit ()
  (if (in:ismousecaptured)
      (if (null mousecapturestate)
	  (progn
	    (in:delta)
	    (setf mousecapturestate :justcaptured))
	  (if (eq mousecapturestate :justcaptured)
	      (progn
		(setq mousecapturestate t))))
      (setq mousecapturestate nil)))

(defun toggle-mouse-capture ()
  (if (not (ismousecaptured))
      (progn
	(glfw:set-input-mode :cursor :disabled))
      (progn
	(glfw:set-input-mode :cursor :normal))))

(defun ismousecaptured ()
  (if  (eq :disabled (glfw:get-input-mode :cursor))
       t
       nil))

;; this is a hash table where the sdl key values
;; are the keys and the unicodes are the values
(defparameter sdl-ascii (make-hash-table))

;; helper function for the pressed key list
(defun key-down (info-list)
  (let* ((sdl-key-code (cdr (assoc :key info-list))))
    (setf  (gethash sdl-key-code sdl-ascii)
	   (adjoin sdl-key-code (gethash sdl-key-code sdl-ascii)))
    (setf in::down-keys (adjoin sdl-key-code in::down-keys))))

;; yet another helper function for the key list
(defun key-up (info-list) 
  (let* ((sdl-key-code (cdr (assoc :key info-list)))
	 (dem-keys (gethash sdl-key-code sdl-ascii)))
    (setf in::down-keys
	  (set-difference in::down-keys dem-keys :test #'eq))))

;; a package which handles the keyboard and mouse
;; mostly just wrappers around SDL

;; this is a list of all the pressed keys,
;; in common lisp char format

(defvar down-keys-prev nil)
(defvar down-keys nil)
(defvar pressed-keys nil)
(defvar released-keys nil)

(defvar key-released-hook-hash nil)
(defvar key-pressed-hook-hash nil)
(defvar key-pressing-hook-hash nil)

(defvar mouse-down-buttons-prev nil)
(defvar mouse-down-buttons nil)
(defvar mouse-pressed-buttons nil)
(defvar mouse-released-buttons nil)

(defvar mouse-button-released-hook-hash nil)
(defvar mouse-button-pressed-hook-hash nil)
(defvar mouse-button-pressing-hook-hash nil)

(defun initialize ()
  (setq down-keys-prev nil)
  (setf down-keys nil)
  (setq pressed-keys nil)
  (setq released-keys nil)
  (setq mouse-down-buttons-prev nil)
  (setq mouse-down-buttons nil)
  (setq mouse-pressed-buttons nil)
  (setq mouse-released-buttons nil)

  (clear-functions))

(defun clear-functions ()
  
  (setq key-released-hook-hash (make-hash-table))
  (setq key-pressed-hook-hash (make-hash-table))
  (setq key-pressing-hook-hash (make-hash-table))
  (setq mouse-button-released-hook-hash (make-hash-table))
  (setq mouse-button-pressed-hook-hash (make-hash-table))
  (setq mouse-button-pressing-hook-hash (make-hash-table)))

;; getting the mouse x position
(defun x () (elt (glfw:get-cursor-position) 0))

;; getting the mouse y position
(defun y () (elt (glfw:get-cursor-position) 1))

(defparameter prev (vector 0 0))
;; returns a 2d vector corresponding to the change in 
;; mouse position
(defun delta ()
  (let ((newx (x))
	(newy (y)))
    (prog1
	(vector
	 (- newx (elt prev 0))
	 (- newy (elt prev 1)))
      (setf prev (vector newx newy)))))

(defun mouse-button-p (button) (member button mouse-down-buttons))
(defun mouse-button-pressing-hook (button func)
  (setf (gethash button mouse-button-pressing-hook-hash) func))
(defun mouse-button-pressed-p (button)
  (member button mouse-pressed-buttons))
(defun mouse-button-pressed-hook (button func)
  (setf (gethash button mouse-button-pressed-hook-hash) func))
(defun mouse-button-released-p (button)
  (member button mouse-released-buttons))
(defun mouse-button-released-hook (button func)
  (setf (gethash button mouse-button-released-hook-hash) func))

;; char to key conversion testing of shit 
(defun key-p (the-key) (member the-key down-keys))
(defun key-pressing-hook (key func)
  (setf (gethash key key-pressing-hook-hash) func))
(defun key-pressed-p (the-key) (member the-key pressed-keys))
(defun key-pressed-hook (key func)
  (setf (gethash key key-pressed-hook-hash) func))
(defun key-released-p (the-key) (member the-key released-keys))
(defun key-released-hook (key func)
  (setf (gethash key key-released-hook-hash) func))

;;pixel = a key on the keyboard or a button on the mouse

;;pixel just pressed
(defun p+1 (id func)
  (if (numberp id)
      (mouse-button-pressed-hook id func)
      (key-pressed-hook id func)))
;;pixel being pressed
(defun p0 (id func)
  (if (numberp id)
      (mouse-button-pressing-hook id func)
      (key-pressing-hook id func)))
;;pixel just released
(defun p-1 (id func)
  (if (numberp id)
      (mouse-button-released-hook id func)
      (key-released-hook id func)))

(defun buttonshit (button state x y)
  (if (eq :pressed state)
      (setf mouse-down-buttons (remove button mouse-down-buttons))
      (setf mouse-down-buttons (adjoin button mouse-down-buttons)))
  (if (or (eq button 5) (eq button 4))
      (if (eq :pressed state)
	  (execute (gethash button mouse-button-released-hook-hash))
	  (execute (gethash button mouse-button-pressed-hook-hash)))))

(defun execute (func)
  (if func
      (funcall func)))

(defun aux (a b)
  (mapcar (lambda (x) (let ((func
			     (gethash x a)))
			(execute func))) b))

(defun update ()
  (setf pressed-keys (set-difference down-keys down-keys-prev))
  (setf released-keys (set-difference down-keys-prev down-keys))
  (setf down-keys-prev down-keys)

  (setf mouse-pressed-buttons
	(set-difference mouse-down-buttons mouse-down-buttons-prev))
  (setf mouse-released-buttons
	(set-difference mouse-down-buttons-prev mouse-down-buttons))
  (setf mouse-down-buttons-prev mouse-down-buttons)
  (pull-hooks))

(defun pull-hooks ()
  (aux key-released-hook-hash released-keys)
  (aux key-pressed-hook-hash pressed-keys) 
  (aux mouse-button-released-hook-hash mouse-released-buttons)
  (aux mouse-button-pressed-hook-hash mouse-pressed-buttons)
  (pulling-hooks))

(defun pulling-hooks ()
  (aux key-pressing-hook-hash down-keys)
  (aux mouse-button-pressing-hook-hash mouse-down-buttons))

(defvar height 256)
(defvar pushed-height -1)
(defvar width 256)
(defvar pushed-width -1)
(defvar caption "default")
(defvar little-caption "default little caption")

(defun akeydown (name)
  (member name down-keys))

(defmacro progno (&body fuck) (declare (ignore fuck)))

(defun push-dimensions (&optional (resizable nil))
  (glfw:set-window-size width height)
  (setq pushed-width width
	pushed-height height))

(defun init ())

(defun dead ())

(defun push-titles ()
  (glfw:set-window-title caption))

(defun set-caption (newcap)
  (setq caption newcap)
  (push-titles))

(defun set-mirco-caption (n)
  (setq little-caption n)
  (push-titles))

(defun update-display ()
  (glfw:swap-buffers))

(defun set-vsync (bool)
  (if bool
      (glfw:swap-interval 1) ;;1 is on
      (glfw:swap-interval 0))) ;;0 is off
