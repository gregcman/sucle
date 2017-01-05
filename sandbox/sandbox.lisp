(in-package #:sandbox)

;;;stage one: getting the initial arguments, 
;;;binding the correct repl io streams
;;;handing off to the next phase
(defparameter original-args nil)
(defparameter handoff-one nil)
(defparameter global-output nil) ;bound to the repl stream
(defparameter global-input nil) ;bound to the repl stream, not the regular
;;;if main is called from the repl, bind these streams
;;;so they can be printed to and read from
(defun bind-repl-streams ()
  (setf global-output *standard-output*)
  (setf global-input *standard-input*))
;;;main is the application "entry point"
(defun main (&rest args)
  (bind-repl-streams)
  (setf original-args args)
  (funcall handoff-one))

;;;stage two: initialize global libraries
;;;add initial arguments to global library
;;;decide whether to stay in the main thread [forever?] or free up the repl
(defparameter handoff-two nil)
(defparameter default-args
  '(:main nil :vsync t))
;;;put the arguments passed to main in the global argument library
(defun put-global-args (argument-list)
  (let ((node argument-list))
    (tagbody
       rep
       (unless (null node)
	 (lset *g/args* (first node) (second node))
	 (setf node (cddr node))
	 (go rep)))))
;;;in some environments [mac os] the opengl thread has to be the same thread
;;;as the main application thread. however, the default "main" is nil
;;;which means opengl can be called from other threads
(defun designate-initial-threads (func)
  (if (lget *g/args* :main)
      (funcall func)
      (lcreate-thread :son-of-main func)))
(eval-when (:compile-toplevel :load-toplevel :execute) 
  (setf handoff-one
	(lambda () (initlib)
		(put-global-args default-args)
		(put-global-args original-args)
		(designate-initial-threads handoff-two))))

;;;stage 3: initialize window and opengl
(defparameter handoff-three nil)
(eval-when (:compile-toplevel :load-toplevel :execute) 
  (setf handoff-two
	(lambda ()
	  (sb-int:set-floating-point-modes :traps nil) ;for some odd reason
	  (window:arise)
	  (funcall window:wrapper handoff-three))))

;;;stage 4: load initial assets
;;;initialize window options, keyboard, renderer, physics
(defparameter handoff-four nil)
;;;by now all the initilization code is to be loaded. the only thing left to do is
;;;to launch the thread which execute the program: renderer and physics
(eval-when (:compile-toplevel :load-toplevel :execute) 
  (setf handoff-three
	(lambda ()
	  (load-assets) ;assets 
	  (setq out:width (if nil 512 854) out:height (if nil 512 480)) ;;window
	  (out:push-dimensions nil)
	  (in::initialize);keyboard
	  (glinnit) ;opengl
	  (physinnit) ;physics
	  (funcall handoff-four))))

;;;stage 5: launch the main constituent threads:
;;the renderthread, if vsync is on, gets locked at a certain fps. The camera and looking around is
;;synced with it for smooth looking
;;;the physics thread can do whatever it wants
(eval-when (:compile-toplevel :load-toplevel :execute) 
  (setf handoff-four
	(lambda ()
	  (setf phystimer (timer))
	  (setf rendertimer (timer))  
	  (unwind-protect (hairy-programs)
	    (cleanup)))))
(defparameter alivep nil)
(defun alive? ()
  (and alivep
       (not window:status)))
(defun hairy-programs ()
  (setq alivep t)
  (lcreate-thread :phys #'physthread)
  (injection))
(defun cleanup ()
  (setq alivep nil)
  (sb-thread:terminate-thread (lget *g/thread* :phys)))
(defparameter render-delay nil)
(defparameter rendertimer nil)
(defparameter renderrate nil)
(defun injection ()
  (if (in:ismousecaptured)
      (look-around))
  (multiple-value-bind (val happened? difference)
      (funcall rendertimer render-delay #'render)
    (declare (ignorable val))
    (when happened?
      (setf renderrate difference)))
  (funcall window:base-needs)
  (when (alive?) 
    (injection)))
(defparameter tick-delay nil)
(defparameter phystimer nil)
(defparameter physrate nil)
(defun physthread ()
  (sb-int:set-floating-point-modes :traps nil);;;;;;;;WHATHAHWHTAHWTAHTAHWTWHATH
  (multiple-value-bind (val happened? difference)
      (funcall phystimer tick-delay #'physics)
    (declare (ignorable val))
    (when happened?
      (setf physrate difference)))
  (when (alive?)
    (physthread)))
