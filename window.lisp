(in-package #:window)

;;only one window is allowed in sdl. But why would anyone want
;;more than tha?
(defparameter window nil)

(defmacro generate-case-events (event-name status &body events)
  `(loop until (= 0 (sdl-cffi::SDL-Poll-Event ,event-name)) do
	(case (sdl::event-type ,event-name)
	  (:QUIT-EVENT (progn (setf ,status t)))
	  ,@(mapcar (lambda (event)  
		      (sdl::expand-event
		       event-name
		       (car event)
		       (gethash (car event) sdl::*events*)
		       (cadr event)
		       (cddr event)))
		    events)))) 

(defparameter status nil)
(defparameter wrapper nil)
(defparameter base-needs nil)

(defparameter mousecapturestate nil)
(defun arise
    (&key
       ((:event-obj sdl-event) (sdl::new-event)))
  (setq status nil)
  (setq window nil)
  (setq mousecapturestate nil)
  (lambda () status)
  (setq base-needs
	(lambda ()
	  (generate-case-events sdl-event status
	    ;;we map the sdl ascii characters to lisp characters
	    (:key-up-event
	     (:state state :scancode scancode
		     :key key :mod mod :unicode unicode)
	     (key-up (pairlis
		      '(:state :scancode :key :mod :unicode)
		      (list state scancode key mod unicode))))
	    (:key-down-event
	     (:state state :scancode scancode
		     :key key :mod mod :unicode unicode)	
	     (key-down (pairlis
			'(:state :scancode :key :mod :unicode)
			(list state scancode key mod unicode))))   
	    (:mouse-button-up-event
	     (:button button :state state :x x :y y)
	     (in::buttonshit button state x y))
	    (:mouse-button-down-event
	     (:button button :state state :x x :y y)
	     (in::buttonshit button state x y))
	    (:video-resize-event
	     (:w w :h h)
	     (setq out:width w)
	     (setq out:height h)))
	  (in::update)
	  (more-window-shit)))
  (setq wrapper
	(lambda (func)				
	  #+darwin (lispbuilder-sdl-cocoahelper::cocoahelper-init)
	  ;;cocoa for macosx
	  ;;Turning off floating point errors for
	  ;;copatibility or some shit
	  #+sbcl(sb-int:set-floating-point-modes :traps '())
	  (sdl:with-init ()  
	    (out::init)
	    (setf
	     cl-opengl-bindings:*gl-get-proc-address*
	     #'sdl-cffi::sdl-gl-get-proc-address)
	    
	    (setf sdl::*sdl-event* sdl-event)
	    (unwind-protect
		 (wowwow func)
	      (out::dead)
	      (sdl:free-event sdl-event))))))

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
  (sdl:show-cursor t)
  (sdl:sdl-wm-grab-input :sdl-grab-off))

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
	(sdl:show-cursor nil)
	(sdl:sdl-wm-grab-input :sdl-grab-on))
      (progn
	(sdl:show-cursor t)
	(sdl:sdl-wm-grab-input :sdl-grab-off))))

(defun ismousecaptured ()
  (if  (eq :sdl-grab-on (sdl:sdl-wm-grab-input :sdl-grab-query))
       t
       nil))

;; this is a hash table where the sdl key values
;; are the keys and the unicodes are the values
(defparameter sdl-ascii (make-hash-table))

;; helper function for the pressed key list
(defun key-down (info-list)
  (let* ((unicode (cdr (assoc :unicode info-list)))
	 (sdl-key-code (cdr (assoc :key info-list)))
	 (character (code-char unicode)))
    (setf  (gethash sdl-key-code sdl-ascii)
	   (adjoin character (gethash sdl-key-code sdl-ascii)))
    (setf in::down-keys (adjoin character in::down-keys))))

;; yet another helper function for the key list
(defun key-up (info-list) 
  (let* ((sdl-key-code (cdr (assoc :key info-list)))
	 (dem-keys (gethash sdl-key-code sdl-ascii)))
    (setf in::down-keys
	  (set-difference in::down-keys dem-keys :test #'char=))))

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
  (sdl:enable-unicode)
  (setq down-keys-prev nil)
  (setf down-keys nil)
  (setq pressed-keys nil)
  (setq released-keys nil)
  (setq mouse-down-buttons-prev nil)
  (setq mouse-down-buttons nil)
  (setq mouse-pressed-buttons nil)
  (setq mouse-released-buttons nil)

  (clear-functions)
  )

(defun clear-functions ()
  
  (setq key-released-hook-hash (make-hash-table))
  (setq key-pressed-hook-hash (make-hash-table))
  (setq key-pressing-hook-hash (make-hash-table))
  (setq mouse-button-released-hook-hash (make-hash-table))
  (setq mouse-button-pressed-hook-hash (make-hash-table))
  (setq mouse-button-pressing-hook-hash (make-hash-table)))

;; getting the mouse x position
(defun x () (sdl:mouse-x))

;; getting the mouse y position
(defun y () (sdl:mouse-y))

;; returns a 2d vector corresponding to the change in 
;; mouse position
(defun delta () (sdl:mouse-relative-position))

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
  (if (zerop state)
      (setf mouse-down-buttons (remove button mouse-down-buttons))
      (setf mouse-down-buttons (adjoin button mouse-down-buttons)))
  (if (or (= button 5) (= button 4))
      (if (zerop state)
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

(defun push-dimensions ()
  (setq window
	(sdl:window width height
		    :opengl t
		    :opengl-attributes
		    '((:sdl-gl-depth-size 16)
		      (:sdl-gl-doublebuffer 1))
		    :resizable nil))
  (setq pushed-width width
	pushed-height height))

(defun init ())

(defun dead ())

(defun push-titles ()
  (sdl:set-caption caption little-caption))

(defun set-caption (newcap)
  (setq caption newcap)
  (push-titles))

(defun set-mirco-caption (n)
  (setq little-caption n)
  (push-titles))
