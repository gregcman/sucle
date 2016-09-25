(in-package #:sandbox)

(defmacro progno (&rest nope))

(defun loud-thread (func name)
  "makes a thread that reads and writes to stdio"
  (sb-thread:make-thread ;; thread function
   #'(lambda (standard-output standard-input)
       ;; thread-local dynamic binding of special variable
       (let ((*standard-output* standard-output) (*standard-input* standard-input))
	 (funcall func)))
   ;; thread function argument, provided by the current thread
   :arguments (list *standard-output* *standard-input*)
   :name name))

(defun timer ()
  (let ((prevtime (get-internal-real-time)))
    (lambda (time afunc)
      (let* ((now (get-internal-real-time))
	     (diff (- now prevtime)))
	(if (> diff time)
	    (progn
	      (setf prevtime now)
	      (funcall afunc)
	      diff)
	    nil)))))

(defun main (&rest args)
  "application entry point"
  (window:arise)
  (setq dathread nil)
  (if (not args)
      (setq dathread (loud-thread (lambda () (funcall window:wrapper #'init)) "ourthread")) 
      (funcall window:wrapper #'init)))

(defparameter dathread nil)

(defparameter kill-button t)
(defun init ()
  "a mess of shit"
  (setq kill-button t)
  (setq out:width (if nil 512 854) out:height (if nil 512 480))
  (out:push-dimensions)
  (in::initialize)
  (glinnit)
  (in:p-1 #\q (let ((variabl nil))
		(lambda ()
		  (setf variabl (not variabl))
		  (if variabl
		      (gl:polygon-mode :front-and-back :line)
		      (gl:polygon-mode :front-and-back :fill)))))
  (in:p+1 #\ (lambda () (setq kill-button nil)))
  (in:p+1 #\v (lambda () (leresize t)))
  (in:p+1 #\r (function window:toggle-mouse-capture))
  (loadletextures)

  (load-into-texture-library "items.png")
  (load-into-texture-library "grasscolor.png")
  (load-into-texture-library "foliagecolor.png")
  (load-into-texture-library "terrain.png")
  
  (bind-shit "terrain.png")

  (setf (simplecam-pos ourcam) (mat:onebyfour '(0 128 0 0)))
  (setf cameraVelocity (mat:onebyfour '(0 0 0 0)))

  (setf phystimer (timer))
  (setf rendertimer (timer))
  (setf physthread (loud-thread #'physthread "physics"))
  (unwind-protect
       (injection)
    (sb-thread:terminate-thread physthread)))

(defun load-into-texture-library (name &optional (othername name))
  (let ((thepic (gethash name picture-library)))
    (if thepic
	(let ((dims (array-dimensions thepic)))
	    (load-shit
	     (fatten thepic)
	     othername (first dims) (second dims))))))

(defun leresize (option)
  (out:push-dimensions option)
  (gl:viewport 0 0 out:width out:height))

(defparameter rendertimer nil)
(defparameter renderrate nil)
(defun injection ()
  (let ((arate (funcall rendertimer
		 (/ 1000.0 100)
		 (lambda ()
		   (funcall window:base-needs)
		   (draw)))))
    (if arate
	(setf renderrate arate)))
  (if 
   (and
    kill-button
    (not window:status))
   (injection)))

(defun caption-info ()
  (window:set-caption
   (concatenate 'string
		in::pressed-keys '(#\:)
		in::down-keys '(#\:)
		in::released-keys)))

(defparameter ourcam (make-simplecam))

(defun draw ()
  (caption-info)
  (render ourcam)
  (sdl:update-display))

(defparameter ticks/sec 60)
(defparameter tickscale (/ 20 ticks/sec))
(defparameter phystimer nil)
(defparameter physrate nil)
(defparameter physthread nil)
(defun physthread ()
  (let ((arate (funcall phystimer
		 (/ 1000.0 ticks/sec)
		 (lambda ()
		   (if (in:ismousecaptured)
		       (controls ourcam))
		   (physics ourcam)))))
    (if arate
	(setf physrate arate)))
  (if (and
       kill-button
       (not window:status))
      (physthread)))
