(in-package :sucle)

(defun start ()
  (app:enter 'sucle-app))

(defun sucle-app ()
  #+nil
  (setf (entity-fly? *ent*) nil
	(entity-gravity? *ent*) t)
  (window:set-vsync t)
  (fps:set-fps 60)
  (ncurses-clone-for-lem:init)
  (menu:use *start-menu2*)
  (app:push-mode 'menu:tick)

  ;;(sucle-mp:with-initialize-multiprocessing)
  (app:default-loop))

;;;;************************************************************************;;;;

(defparameter *entities* nil)
(defparameter *ent* nil)
(defparameter *fist* (make-fist))
(defparameter *reach* 50.0)
(defparameter *sky-color* (let ((n 0.5))
			    `(,n ,n ,n))
  )

;;;;;

(defparameter *session* nil)
(defparameter *ticks* 0)
(defparameter *fov* (floatify (* pi (/ 85 180))))
(defparameter *camera*
  (camera-matrix:make-camera
   :frustum-far (* 256.0)
   :frustum-near (/ 1.0 8.0)))

;;*frame-time* is for graphical frames, as in framerate.
(defparameter *frame-time* 0)
(defun sucle-per-frame ()
  (incf *frame-time*)

  ;;set the chunk center aroun the player
  (livesupport:update-repl-link)
  (application:on-session-change *session*
    (setf *entities* (loop :repeat 10 :collect (create-entity
						 (create-aabb 0.3 0.12 0.3 -0.3 -1.5 -0.3))))
    (setf *ent* (elt *entities* 0))

    ;;Controller?
    (reset-all-modes)
    (enable-mode :normal-mode)
    (enable-mode :god-mode)
    ;;(setf voxel-chunks::*voxels* (voxel-chunks::make-voxels))
    ;;(fix::seed)
    )
  (gl:polygon-mode :front-and-back :line)

  ;;Polling
  ;;Physics
  ;;Rendering Chunks
  ;;Rendering Other stuff
  ;;Meshing
  ;;Waiting on vsync
  ;;Back to polling
  
  ;;Physics and Polling should be close together to prevent lag
  
  ;;physics

  (mvc 'standard-fist
       (spread (entity-position *ent*))
       ;;FIXME::conses a new vec
       (spread (sb-cga:vec*
		(camera-matrix:camera-vec-forward *camera*)
		*reach*))
       *fist*)
  (when (mode-enabled-p :fist-mode)
    (run-buttons *fist-keys*))
  (when (mode-enabled-p :god-mode)
    (run-buttons *god-keys*))
  (when (mode-enabled-p :movement-mode)
    ;;Set the sneaking state
    (setf (entity-sneak? *ent*)
	  (cond
	    ((window:button :key :down :left-shift)
	     0)
	    ((window:button :key :down :left-control)
	     1)))
    ;;Jump if space pressed
    (setf (entity-jump? *ent*)
	  (window:button :key :down #\Space))
    (when (window:button :key :pressed #\Space)
      (set-doublejump *ent*))
    ;;Set the direction with WASD
    (setf
     (entity-hips *ent*)
     (let ((x 0)
	   (y 0))
       (when (window:button :key :down #\w)
	 (incf x))
       (when (window:button :key :down #\s)
	 (decf x))
       (when (window:button :key :down #\a)
	 (decf y))
       (when (window:button :key :down #\d)
	 (incf y))
       ;;[FIXME]
       ;;This used to be cached and had its own function in
       ;;the control.asd
       (if (and (zerop x)
		(zerop y))
	   nil			   
	   (floatify (atan y x)))))
    ;;update the internal mouse state
    ;;taking into consideration fractions
    (when (window:mouse-locked?)
      (update-moused 1.0)))
  (when (mode-enabled-p :normal-mode)
    ;;[FIXME] because this runs after update-moused, the camera swivels
    ;;unecessarily.
    (run-buttons *normal-keys*))
  (let ((number-key (control:num-key-jp :pressed)))
    (when number-key
      (setf *ent* (elt *entities* number-key))))
  
  ;;Set the pitch and yaw of the player based on the
  ;;mouse position
  (mvc 'increment-neck-values (entity-neck *ent*) (neck-values))

  ;;Run the game ticks

  ;;FIXME:: run fps:tick if resuming from being paused.
  (multiple-value-bind (fraction-for-fps game-ticks-per-iteration)
      (fps:tick
	(incf *ticks*)
	;;run the physics
	(run-physics-for-entity *ent*))
    (declare (ignorable game-ticks-per-iteration))
    ;;render chunks and such
    ;;handle chunk meshing
    (sync_entity->camera *ent* *camera* fraction-for-fps)
    
    (draw-to-default-area)
    ;;this also clears the depth and color buffer.
    (apply #'render-sky *sky-color*)
    (render-chunks::use-chunk-shader :camera *camera*)
    (progn ;time
     (render-chunks::render-chunks))

    
    ;;selected block and crosshairs
    (use-solidshader *camera*)
    (render-total-bounding-area)
    (render-fist *fist*)
    ;;(render-debug fraction-for-fps)
    (render-crosshairs)
    )

  )

;;;;************************************************************************;;;;
;;;;controls

(defparameter *normal-keys*
  `(((:key :pressed :escape) .
     ,(lambda ()
	(window:get-mouse-out)
	(app:pop-mode)))
    ((:key :pressed #\e) .
     ,(lambda ()
	(cursor-motion-difference)
	(window:toggle-mouse-capture)
	(set-mode-if :movement-mode (not (window:mouse-free?)))
	(set-mode-if :fist-mode (not (window:mouse-free?)))
	;;Flush changes to the mouse so
	;;moving the mouse while not captured does not
	;;affect the camera
	;;FIXME::not implemented.
	;;(moused)
	))))
(defparameter *god-keys*
  `(;;Toggle noclip with 'v'
    ((:key :pressed #\v) .
     ,(lambda () (toggle (entity-clip? *ent*))))
    ;;Toggle flying with 'f'
    ((:key :pressed #\f) .
     ,(lambda () (toggle (entity-fly? *ent*))
	      (toggle (entity-gravity? *ent*))))))
(defparameter *block* 1)
(defparameter *fist-keys*
  `(((:mouse :pressed :left) . 
     ,(lambda ()
	(when (fist-exists *fist*)	  
	  (multiple-value-bind (x y z) (spread (fist-selected-block *fist*))
	    (setf (voxel-chunks::getobj x y z) 0)))))
    ;;#+nil
    ((:mouse :pressed :right) .
     ,(lambda ()
	(when (fist-exists *fist*)
	  (multiple-value-bind (x y z) (spread (fist-normal-block *fist*))
	    (setf (voxel-chunks::getobj x y z) *block*)))))
    ((:mouse :pressed :middle) .
     ,(lambda ()
	(when (fist-exists *fist*)
	  (multiple-value-bind (x y z) (spread (fist-selected-block *fist*))
	    (multiple-value-bind (xn yn zn) (spread (fist-normal-block *fist*))
	      (middlemouse x y z xn yn zn))))))))

(defparameter *fun* (constantly nil))
(defvar *normals* ())
(defun middlemouse (x y z xn yn zn)
  (declare (ignorable x y z))
  (block out
    (handler-bind ((error
		    (lambda (c)
		      (declare (ignorable c))
		      (return-from out))))
      (let ((*normals* (list (- xn x) (- yn y) (- zn z))))
	(funcall *fun* x y z)))))
