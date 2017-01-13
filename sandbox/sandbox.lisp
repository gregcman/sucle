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
  '(:main nil :vsync t :window-width 854 :window-height 480))
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
  (defun handoff-one ()
    (initlib)
    (put-global-args default-args)
    (put-global-args original-args)
    (designate-initial-threads handoff-two))
  (setf handoff-one #'handoff-one))

;;;stage 3: initialize window and opengl
(defparameter handoff-three nil)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun handoff-two ()
    (sb-int:set-floating-point-modes :traps nil) ;for some odd reason
    (window:init)
    (window::wrapper handoff-three))
  (setf handoff-two #'handoff-two))

;;;stage 4: load initial assets
;;;initialize window options, keyboard, renderer, physics
(defparameter handoff-four nil)
;;;by now all the initilization code is to be loaded. the only thing left to do is
;;;to launch the thread which execute the program: renderer and physics
(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defun handoff-three ()
    (load-assets) ;assets
    (let ((width (lget *g/args* :window-width))
	  (height (lget *g/args* :window-height)))
      (window:push-dimensions width height))
    (glinnit) ;opengl
    (physinnit) ;physics
    (funcall handoff-four))
  (setf handoff-three #'(lambda () (wowwow #'handoff-three))))

;;;stage 5: launch the main constituent threads:
;;the renderthread, if vsync is on, gets locked at a certain fps. The camera and looking around is
;;synced with it for smooth looking
;;;the physics thread can do whatever it wants
(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defun handoff-four ()
    (setf phystimer (timer:timer))
    (setf rendertimer (timer:timer))  
    (unwind-protect (hairy-programs)
      (cleanup)))
  (setf handoff-four #'handoff-four))

(defparameter alivep nil)
(defun alive? ()
  (and alivep
       (not window:*status*)))
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

(defparameter fps-func (averager 256))
(defparameter fps nil)
(defun injection ()
  (multiple-value-bind (val happened? difference)
      (funcall rendertimer render-delay (lambda ()(window:poll);;where to put?
						  (remove-spurious-mouse-input)
						  (render) (physics)))
    (declare (ignorable val))
    (when happened?
      (setf renderrate difference)
      (setf fps (/ 1000000.0 (funcall fps-func difference)))
      (window:set-caption (format nil "~2,4$" fps)))
    (if happened?
	(set-render-cam-pos 0)
	(set-render-cam-pos (min difference tick-delay))))
  (when (alive?) 
    (injection)))

(defun wowwow (func)
  (tagbody
     (handler-bind
	 ((error
	   (lambda (condition)
	     (print condition)
	     (window::get-mouse-out)
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


;;;;when the renderer and physics go together its as smooth as ice cream


(defparameter tick-delay nil)
(defparameter phystimer nil)
(defparameter physrate nil)
(defun physthread ()

  (progno (multiple-value-bind (val happened? difference)
	      (funcall phystimer tick-delay #'physics)
	    (declare (ignorable val))
	    (if happened?
		(set-render-cam-pos 0)
		(set-render-cam-pos (min difference tick-delay)))
	    (when happened?
	      (setf physrate difference))))
  (when (alive?)
    (progno (physthread))))

(defun world-setup ()
  (clean-dirty)
  (world:setup-hashes))

(world-setup)

