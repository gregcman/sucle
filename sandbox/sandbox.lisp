(in-package #:sandbox)

(defparameter init-hook (hook:create-hook))

(defun main ()
  (hook:run-hook init-hook)
  (designate-initial-threads handoff-two))

(defparameter handoff-two nil)

(defparameter ourthread nil)

;;;in some environments [mac os] the opengl thread has to be the same thread
;;;as the main application thread. however, the default "main" is nil
;;;which means opengl can be called from other threads
 (defun designate-initial-threads (func)
  (if nil
      (funcall func)
      (setf ourthread (sb-thread:make-thread func :name "son-of-main"))))

;;;stage 3: initialize window and opengl
(defparameter handoff-three nil)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun handoff-two ()
    (sb-int:set-floating-point-modes :traps nil) ;for some odd reason
    (window:init)
    (window::wrapper handoff-three))
  (setf handoff-two #'handoff-two))


(defparameter handoff-four nil)
(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defun handoff-three ()
    (let ((width 854)
	  (height 480))
      (window:push-dimensions width height))
    (unless voxel-world-setup?
      (setf voxel-world-setup? t)
      (world-setup))
    (glinnit) ;opengl
    (physinnit) ;physics
    (funcall handoff-four))
  (setf handoff-three #'handoff-three))

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defun handoff-four ()
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
  (injection))
(defun cleanup ()
  (setq alivep nil))

(defun injection ()
  (window:poll)
  (physics)
  (set-render-cam-pos 0)
  (remove-spurious-mouse-input)
  (render)
  (when (alive?) 
    (injection)))

(defparameter tick-delay nil)
(defparameter voxel-world-setup? nil)

(defun world-setup ()
  (clean-dirty)
  (world:setup-hashes))
