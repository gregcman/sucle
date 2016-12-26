(in-package #:sandbox)

;;spawn two threads
;;physics thread
;;rendering thread

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
  (setq kill-button t)
  (setq out:width (if nil 512 854) out:height (if nil 512 480))
  (out:push-dimensions nil)
  (in::initialize)
  (glinnit)
  (physinnit)
  
  (in:p+1 :ESCAPE (lambda () (setq kill-button nil)))
  ;;escape to quit
  (in:p+1 :E (function window:toggle-mouse-capture))
  ;;e to escape mouse

  (setf phystimer (timer))
  (setf rendertimer (timer))
  (setf physthread (loud-thread #'physthread "physics"))
  (unwind-protect
       (injection)
    (sb-thread:terminate-thread physthread)))

(defparameter rendertimer nil)
(defparameter renderrate nil)
(defun injection ()
  (let ((arate (funcall rendertimer
			(if nil
			    0
			    (/ 1000000.0 59.88))
			(lambda ()
			  (render)))))
    (if arate
	(progn
	  (setf renderrate arate)
	  (window:set-caption (write-to-string (/ renderrate 1000.0))))))
  (funcall window:base-needs)
  (progn
    (if (in:ismousecaptured)
	(look-around)))
  (if 
   (and
    kill-button
    (not window:status))
   (injection)))

(defparameter ticks/sec 60)
(defparameter tickscale (/ 20 ticks/sec))
(defparameter phystimer nil)
(defparameter physrate nil)
(defparameter physthread nil)
(defun physthread ()
  (let ((arate (funcall phystimer
			(/ 1000000.0 ticks/sec)
			(lambda ()
			  (if (in:ismousecaptured)
			      (controls))
			  (physics)))))
    (if arate
	(setf physrate arate)))
  (if (and
       kill-button
       (not window:status))
      (physthread)))
