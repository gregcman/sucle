
(in-package :sucle)

;;;;************************************************************************;;;;
;;;;<BOXES?>
(defun create-aabb (&optional (maxx 1.0) (maxy maxx) (maxz maxx)
		      (minx (- maxx)) (miny (- maxy)) (minz (- maxz)))
  (aabbcc:make-aabb
   :minx minx
   :maxx maxx
   :miny miny
   :maxy maxy
   :minz minz
   :maxz maxz))

(defparameter *block-aabb*
  ;;;;1x1x1 cube
  (create-aabb 1.0 1.0 1.0 0.0 0.0 0.0))

(defparameter *slab-aabb*
  ;;;;slab
  (create-aabb 1.0  #+nil 0.5 1.0 1.0 0.0 0.0 0.0))

;;;;[FIXME]The point of this is to reduce the amount of bits to store the hitbox.
;;;;Why? because when there is an inexact number, like 0.3, there are bits at the end which
;;;;get chopped off or something, thus leading to strange clipping.
;;;;This effectively reduces the precision, giving leeway for math operations.
;;;;My prediction could be wrong though.
(defun round-to-nearest (x &optional (n (load-time-value (/ 1.0 128.0))))
  (* n (round (/ x n))))
(defparameter *player-aabb*
  (apply #'create-aabb
	 (mapcar 'round-to-nearest	 
		 '(0.3 0.12 0.3 -0.3 -1.5 -0.3))))

;;;a very small cubic fist
(defparameter *fist-aabb* (create-aabb 0.00005))

(defparameter *chunk-aabb*
  (apply 'create-aabb
	 (mapcar 'floatify
		 (list
		  voxel-chunks:*chunk-size-x*
		  voxel-chunks:*chunk-size-y*
		  voxel-chunks:*chunk-size-z*
		  0.0
		  0.0
		  0.0))))

(defparameter *big-fist-aabb*
  (create-aabb
   ;;0.5
   ;;1.5
   8.0))
;;;;</BOXES?>

(defun configure-world-path
    (&optional
       (world
	;;"first/"
	;;#+nil
	;;"test/"
	"other/"
	;;"third/"
	;;"ridikulisp/"
	)
       (working-dir
	(sucle-temp:path "save/")
	#+nil
	(cdr (assoc (machine-instance) 
		    '(("gm3-iMac" . #P"/media/imac/share/space/lispysaves/saves/sandbox-saves/")
		      ("nootboke" . #P"/home/terminal256/Documents/saves/"))
		    :test 'equal))))
  (setf world:*world-directory* world)
  (setf world:*some-saves* working-dir))
(defun start ()
  (configure-world-path)
  (enter 'sucle-app))
(defun sucle-app ()
  #+nil
  (setf (entity-fly? *ent*) nil
	(entity-gravity? *ent*) t)
    ;;(our-load)
  (window:set-vsync t)
  (fps:set-fps 60)
  (ncurses-clone-for-lem:init)
  (push-mode 'menu-mode-per-frame)
  (voxel-chunks:clearworld)
  (setf *ent* (create-entity))
  (sucle-mp:with-initialize-multiprocessing
   (unwind-protect (default-loop)	  
     (when world:*persist*
       (world:msave)))))

;;;;

#+nil
(defun start ()
  (application:main
   (lambda ()
     (call-with-world-meshing-lparallel 
      (lambda ()
	(loop
	   (application:poll-app)
	   (per-frame)))))
   :width 720
   :height 480
   :title "conceptually simple block game"))
#+nil
(defun load-world-again (name)
  (setf world:*persist* nil)
  (setf world:*world-directory* name)
  (load-world t))


;;;;************************************************************************;;;;
(defun enter (&optional (app 'default-per-frame))
  (reset-per-frame-and-stack)
  (subapp app)
  (start-window))

(defun start-window ()
  (application:main 'default-loop
   :width (* 80 8)
   :height (* 25 16)
   :title ""))

(defun default-loop ()
  (loop
     (application:poll-app)
     (per-frame)))
;;Popping the last node of the stack is equivalent to quitting,
;;because of the default-per-frame that quits app.

;;A subapp is like a copy of the app,
;;glfw3 window and all, within the app?

(defun unit-circular-list (item)
  (let ((cell (list item)))
    (setf (cdr cell) cell)
    cell))
(defun circular-per-frame ()
  (unit-circular-list 'default-per-frame))
(defparameter *null-per-frame* (circular-per-frame))
(defparameter *null-app-stack* (unit-circular-list *null-per-frame*))
(defparameter *per-frame* *null-per-frame*)
(defparameter *app-stack* *null-app-stack*)
(defun save-modes-to-app-stack ()
  (push *per-frame* *app-stack*)
  (reset-per-frame))
(defun restore-modes-to-app-stack ()
  (setf *per-frame* (pop *app-stack*)))

(defun subapp (fun)
  ;;The application is both a mode
  ;;and a means of quitting.
  (labels ((this-function ()
	     ;;(print "running")
	     (unwind-protect
		  (progn (save-modes-to-app-stack)
		    (application::with-quit-token ()
		      (funcall fun)))
	       (restore-modes-to-app-stack)
	       (pop-mode))))
    (push-mode #'this-function)))

(defun default-per-frame ()
  ;;Do nothing, except quit
  (application:quit))
;;This is a circular list with one element.
;;So if you keep popping the mode,
;;nothing happens.
(defun reset-per-frame-and-stack ()
  (reset-per-frame)
  (reset-app-stack))
(defun reset-per-frame ()
  (setf *per-frame* *null-per-frame*))
(defun reset-app-stack ()
  (setf *app-stack* *null-app-stack*))
(defun per-frame ()
  (funcall (car *per-frame*)))
(defun push-mode (mode)
  (etypecase mode
    ;;mode is either a function
    (function t)
    ;;or a symbol with a 
    (symbol
     (assert (fboundp mode) nil "Symbol:~a is function unbound" mode)))
  (push mode *per-frame*))
(defun pop-mode ()
  (pop *per-frame*))
(defun switch-mode (mode)
  (pop-mode)
  (push-mode mode))

;;test
(defun test-for-modes ()
  (labels ((app-entry ()
	     (push-mode #'app)
	     (default-loop))
	   (app ()
	     (when (window:button :key :repeat #\u)
	       (print "going up!")
	       (subapp #'app-entry))
	     (when (window:button :key :repeat #\m)
	       (print "another mode")
	       (app-entry))
	     (when (window:button :key :repeat #\n)
	       (print "removing mode")
	       ;;FIXME::will this pop too many modes?
	       (pop-mode)
	       )
	     (when (window:button :key :repeat #\q)
	       (print "quitting")
	       (application:quit))
	     (when (window:button :key :pressed #\p)
	       (let ((*print-circle* t))
		 (print *per-frame*)
		 (print *app-stack*)))))
    (enter #'app-entry)))

;;;;************************************************************************;;;;

(defparameter *raw-mouse-x* 0.0d0)
(defparameter *raw-mouse-y* 0.0d0)
(defun cursor-motion-difference
    (&optional (x window:*mouse-x*) (y window:*mouse-y*))
  ;;Return the difference in position of the last time the
  ;;cursor was observed.
  ;;*raw-mouse-x* and *raw-mouse-y* hold the last value
  ;;of the cursor.
  (multiple-value-prog1
      (values (- x *raw-mouse-x*)
	      (- y *raw-mouse-y*))
    (setf *raw-mouse-x* x
	  *raw-mouse-y* y)))

(defparameter *mouse-x* 0.0d0)
(defparameter *mouse-y* 0.0d0)
(defparameter *lerp-mouse-x* 0.0d0)
(defparameter *lerp-mouse-y* 0.0d0)
(defun update-moused (clamp &optional (smoothing-factor 1.0))
  (multiple-value-bind (dx dy) (cursor-motion-difference)
    (let ((x (+ *mouse-x* dx))
	  (y (+ *mouse-y* dy)))
      ;;So looking straight up stops.
      (when (> y clamp)
	(setf y clamp))
      ;;So looking straight down stops
      (let ((negative (- clamp)))
	(when (< y negative)
	  (setf y negative)))
      (setf *mouse-x* x)
      (setf *mouse-y* y)))
  ;;*lerp-mouse-x* and *lerp-mouse-y* are used
  ;;for camera smoothing with the framerate.
  (setf *lerp-mouse-x* (alexandria:lerp smoothing-factor *lerp-mouse-x* *mouse-x*))
  (setf *lerp-mouse-y* (alexandria:lerp smoothing-factor *lerp-mouse-y* *mouse-y*)))
(defparameter *mouse-multiplier* 0.002617)
(defparameter *mouse-multiplier-aux* (/ (* 0.5 pi 0.9999) *mouse-multiplier*))

(defun unit-pitch-yaw (pitch yaw &optional (result (sb-cga:vec 0.0 0.0 0.0)))
  (let ((cos-pitch (cos pitch)))
    (with-vec (x y z) (result symbol-macrolet)
      (setf x (* cos-pitch (sin yaw))
	    y (sin pitch)
	    z (* cos-pitch (cos yaw)))))
  result)

;;;;
(defparameter *fov* (* (floatify pi) (/ 85 180)))
(defparameter *camera*
  (camera-matrix:make-camera
   :frustum-far (* 256.0)
   :frustum-near (/ 1.0 8.0)))
(defparameter *fog-ratio* 0.75)
(defparameter *time-of-day* 1.0)

(defun update-camera (&optional (camera *camera*))
  (setf (camera-matrix:camera-aspect-ratio camera)
	(/ (floatify window:*width*)
	   (floatify window:*height*)))
  (setf (camera-matrix:camera-fov camera) *fov*)
  (setf (camera-matrix:camera-frustum-far camera) (* 1024.0 256.0))
  (camera-matrix:update-matrices camera))

;;;;************************************************************************;;;;
;;emacs-like modes
(defparameter *active-modes* ())
(defun reset-all-modes ()
  (setf *active-modes* nil))
(defun enable-mode (mode)
  (pushnew mode *active-modes* :test 'equal))
(defun disable-mode (mode)
  (setf *active-modes* (delete mode *active-modes*)))
(defun mode-enabled-p (mode)
  (member mode *active-modes* :test 'equal))
(defun set-mode-if (mode p)
  (if p
      (enable-mode mode)
      (disable-mode mode)))
;;;;************************************************************************;;;;

(defparameter *last-session* nil)
(defparameter *session* nil)
(defparameter *game-ticks-per-iteration* 0)
(defparameter *fraction-for-fps* 0.0)
(defparameter *fist* nil)
(defun sucle-per-frame ()
  ;;[FIXME]where is the best place to flush the job-tasks?
  (sucle-mp:flush-job-tasks)
  ;;set the chunk center aroun the player
  (with-vec (x y z) ((player-position))
    (world:set-chunk-coordinate-center x y z))
  (livesupport:update-repl-link)
  (application:on-session-change *session*
    (reset-all-modes)
    (enable-mode :normal-mode)
    (enable-mode :god-mode)
    (world:load-world t))
  
  ;;Polling
  ;;Physics
  ;;Rendering Chunks
  ;;Rendering Other stuff
  ;;Meshing
  ;;Waiting on vsync
  ;;Back to polling
  
  ;;Physics and Polling should be close together to prevent lag
  
  ;;physics

  ;;Calculate what bocks are selected etc..
  (with-vec (px py pz) ((player-position))
    (with-vec (vx vy vz) ((sb-cga:vec* (camera-matrix:camera-vec-forward *camera*)
				       (* -1.0 *reach*)))
      (setf *fist* (standard-fist px py pz vx vy vz))))
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
    ;;Set the direction with WASD
    (setf
     (entity-hips *ent*)
     (let ((x 0)
	   (y 0))
       (when (window:button :key :down #\w)
	 (decf x))
       (when (window:button :key :down #\a)
	 (decf y))
       (when (window:button :key :down #\s)
	 (incf x))
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
    (update-moused *mouse-multiplier-aux* 1.0))
  (when (mode-enabled-p :normal-mode)
    ;;[FIXME] because this runs after update-moused, the camera swivels
    ;;unecessarily.
    (run-buttons *normal-keys*))

  ;;Run the game ticks

  ;;FIXME:: run fps:tick if resuming from being paused.
  (setf
   (values *fraction-for-fps* *game-ticks-per-iteration*)
   (fps:tick
     (incf *ticks*)
     (setf *time-of-day* 1.0)
     ;;run the physics
     (run-physics-for-entity *ent*)))

  ;;Calculate the camera position from
  ;;the past, current position of the player and the frame fraction
  (set-camera-position *fraction-for-fps*)
  ;;Set the pitch and yaw of the player based on the
  ;;mouse position
  (setf (necking-yaw (entity-neck *ent*))
	(floatify (- (* *lerp-mouse-x* *mouse-multiplier*)))
	(necking-pitch (entity-neck *ent*))
	(floatify (* *lerp-mouse-y* *mouse-multiplier*)))
  ;;Set the direction of the camera based on the
  ;;pitch and yaw of the player
  (unit-pitch-yaw 
   (necking-pitch (entity-neck *ent*))
   (necking-yaw (entity-neck *ent*))
   (camera-matrix:camera-vec-forward *camera*))
  
  (modify-camera-position-for-sneak)
  
  ;;load or unload chunks around the player who may have moved
  (world:load-world)
  ;;render chunks and such
  ;;handle chunk meshing
  (application:on-session-change *last-session*
    (reset-chunk-display-list)
    (update-world-vao))
  ;;update the camera
  (update-camera *camera*)
  (draw-to-default-area)
  ;;this also clears the depth and color buffer.
  (let ((color (the-sky-color)))
    (apply #'render-sky color)
    (use-chunk-shader
     :camera *camera*
     :sky-color color
     :time-of-day *time-of-day*
     :fog-ratio *fog-ratio*
     ))
  (render-chunks)
  (use-occlusion-shader *camera*)
  (render-chunk-occlusion-queries)
  ;;selected block and crosshairs
  (use-solidshader *camera*)
  (render-fist *fist*)
  (render-crosshairs)

  (complete-render-tasks)
  (dispatch-mesher-to-dirty-chunks))

;;;;************************************************************************;;;;
;;Ripped from sucle-test essentially.
(defparameter *view*
  (ncurses-clone:ncurses-newwin 5 50 0 0))
(defun menu-app ()
  (default-loop))
(defun menu-mode-per-frame ()
  (ncurses-clone-for-lem::easy-frame 1 1 10 10 *view*)
  (simulate-menu *menu*)
  (ncurses-clone-for-lem:render :update-data t :win *view*))

(defun draw-string (str x y
		    &key (view *view*) (fg "white") (bg "black")
		      (underline nil) (bold nil) (reverse nil))
  (lem.term:with-attribute
      (:fg fg :bg bg :underline underline :bold bold :reverse reverse)
    (ncurses-clone:ncurses-mvwaddstr view y x str)))


(defun run-button (pair)
  ;;((:key :pressed #\Space) . function)
  (when (apply 'window:button (car pair))
    (funcall (cdr pair))))
(defun run-buttons (pairs)
  (mapc 'run-button pairs))
;;;;MENU
;;-> inspired by html dom?
(defvar *current-menu-data*)
(defvar *menu-height*)
(defvar *menu-width*)
(defun simulate-menu (&optional (menu *menu*))
  ;;do buttons
  (let
      ;;give buttons access to the DOM
      ((*current-menu-data* (menu-data menu))
       (*menu-height* (ncurses-clone:win-lines *view*))
       (*menu-width* (ncurses-clone:win-cols *view*)))
    (run-buttons (menu-buttons menu)))
  ;;do items
  (let ((menu-data (menu-data menu)))
    (dolist (item menu-data)
      (apply 'draw-string (cdr item)))))
(defun menu-buttons (&optional (menu *menu*))
  (first menu))
(defun menu-data (&optional (menu *menu*))
  (second menu))
(defparameter *menu*
  `(;;keys bound to functions
    (((:key :pressed #\f) .
      ,(lambda () (print "Paying Respects")))
     ((:key :pressed #\q) .
      ,(lambda () (application:quit)))
     ((:key :pressed #\Escape) .
      ,(lambda () (application:quit)))
     ((:key :pressed #\p) .
      ,(lambda () (pop-mode)))
     ((:key :pressed #\o) .
      ,(lambda () (push-mode 'menu-mode-per-frame)))
     ((:key :pressed #\s) .
      ,(lambda ()
	 (push-mode 'sucle-per-frame)))
     ((:key :pressed #\c) .
      ,(lambda ()
	 (print "Clearing...")
	 (let ((clear (assoc :clear *current-menu-data*)))
	   (setf (second clear)
		 (with-output-to-string (str)
		   (let ((clearstr
			  (make-string *menu-width*
				       :initial-element #\space)))
		     (dotimes (y *menu-height*)
		       (terpri str)
		       (write-string clearstr str))))))))
     ((:key :released #\c) .
      ,(lambda ()
	 (print "Clearing Done!")
	 (let ((clear (assoc :clear *current-menu-data*)))
	   (setf (second clear)
		 "")))))
    ;;data to render
    ((:hello
      "
Press s to start the game

Press c to clear

Press h for help

Press F to pay respects [not really]

Press q/escape to quit
" 4 4 :bold t)
     ;;(:hello "world" 8 16 :fg "green" :bg "red" :reverse t :bold t)
     (:clear "" 0 0  :bold t))
    ()))
;;;MENU
;;;;************************************************************************;;;;
(defparameter *sky-color*
  (mapcar 'utility:byte/255
	  ;;'(0 0 0)
	  '(173 204 255)))
(defun the-sky-color ()
  (mapcar 
   (lambda (x)
     (alexandria:clamp (* x *time-of-day*) 0.0 1.0))
   *sky-color*))

;;;

;;;detect more entities
;;;detect block types?
(defun not-occupied (x y z &optional (ent *ent*))
  (let ((aabb (entity-aabb ent))
	(pos (pointmass-position
	      (entity-particle ent))))
    (aabbcc:aabb-not-overlap
     (pos-to-block-aabb x y z)
     (floatify x)
     (floatify y)
     (floatify z)
     aabb
     (aref pos 0)
     (aref pos 1)
     (aref pos 2))))

(defparameter *blockid* (block-data:lookup :planks))

(defparameter *ent* nil)
(defparameter *ticks* 0)

(defun player-position ()
  (let* ((player-pointmass (entity-particle *ent*))
	 (curr (pointmass-position player-pointmass)))
    curr))
(defun player-position-old ()
  (let* ((player-pointmass (entity-particle *ent*))
	 (prev (pointmass-position-old player-pointmass)))
    prev))

(defun set-camera-position (fraction)
  (let ((vec (camera-matrix:camera-vec-position *camera*)))
    (nsb-cga:%vec-lerp vec (player-position-old) (player-position) fraction)))
(defun modify-camera-position-for-sneak ()
  (let ((vec (camera-matrix:camera-vec-position *camera*)))
    (when (and (not (entity-fly? *ent*))
	       (eql 0 (entity-sneak? *ent*)))
      (nsb-cga:%vec- vec vec (load-time-value (nsb-cga:vec 0.0 0.125 0.0))))))


(defparameter *reach* 5.0)

;;;;Default punching and placing blocks
(defparameter *left-fist-fnc* 'destroy-block-at)
(defun destroy-block-at (x y z)
  ;;(blocksound x y z)
  (world:plain-setblock x y z (block-data:lookup :air) 15))
(defparameter *right-fist-fnc* 'place-block-at)
(defun place-block-at (x y z &optional (blockval *blockid*))
  (when (not-occupied x y z)
    ;;(blocksound x y z)
    (world:plain-setblock x y z blockval (block-data:data blockval :light))))
;;;;x
(defparameter *x* 0)
(defparameter *y* 0)
(defparameter *z* 0)
(defparameter *fist-keys*
  `(((:mouse :pressed :left) . 
     ,(lambda ()
	(when (fist-exists *fist*)
	  (with-vec (a b c) ((fist-selected-block *fist*))
	    (let ((*x* a)
		  (*y* b)
		  (*z* c))
	      (funcall *left-fist-fnc* a b c))))))
    ((:mouse :pressed :right) .
     ,(lambda ()
	(when (fist-exists *fist*)
	  (with-vec (a b c) ((fist-normal-block *fist*))
	    (let ((*x* a)
		  (*y* b)
		  (*z* c))
	      (funcall *right-fist-fnc* a b c))))))))
(defparameter *normal-keys*
  `(((:key :pressed #\p) .
     ,(lambda () (update-world-vao)))
    ((:key :pressed :escape) .
     ,(lambda ()
	(window:get-mouse-out)
	(pop-mode)))
    ((:key :pressed #\e) .
     ,(lambda ()
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

;;(defparameter *swinging* nil)
#+nil
(progn
  (defparameter *big-fist-fun* (constantly nil))
  (defparameter *middle-fist-fnc* 'place-block-at)
  (defparameter *4-fist-fnc* 'tree)
  (defparameter *5-fist-fnc*
    '5fun))
#+nil
(progn
  (setf *big-fist-fun* 'correct-earth)
  (setf *middle-fist-fnc* 'player-feet-at)
  (setf *middle-fist-fnc* 'line-to-player-feet))
#+nil
(progn
  (when (window:button :key :pressed #\2) 
    (toggle *dirtying2*))
  (when (window:button :key :pressed #\1) 
    (toggle *dirtying*))
  (when (window:button :key :pressed #\3) 
    (toggle *swinging*)))


#+nil
(defparameter *big-fist-reach* 32)
#+nil
(when (window:mouse-locked?)
  (with-vec (px py pz) (pos)
    (with-vec (vx vy vz) (look-vec)
      (when *swinging*
	(let ((u *big-fist-reach*))
	  (aabbcc:aabb-collect-blocks
	      (px py pz (* u vx) (* u vy) (* u vz)
		  *big-fist-aabb*)
	      (x y z contact)
	    (declare (ignorable contact))
	    (let ((*x* x)
		  (*y* y)
		  (*z* z))
	      (funcall *big-fist-fun* x y z)))))))

  )
  #+nil
  (progn
    (when (window:button :mouse :pressed :middle)
      (with-vec (a b c) ((fist-selected-block *fist*))
	(let ((*x* a)
	      (*y* b)
	      (*z* c))
	  (funcall *middle-fist-fnc* a b c))))
    (when (window:button :mouse :pressed :4)
      (with-vec (a b c) ((fist-selected-block *fist*))
	(let ((*x* a)
	      (*y* b)
	      (*z* c))
	  (funcall *4-fist-fnc* a b c))))
    (when (window:button :mouse :pressed :5)
      (with-vec (a b c) ((fist-selected-block *fist*))
	(let ((*x* a)
	      (*y* b)
	      (*z* c))
	  (funcall *5-fist-fnc* a b c)))))

;;;; Changing the color of the sky based on which way we're looking.
#+nil
(defun deg-rad (deg)
  (* deg (load-time-value (utility:floatify (/ pi 180)))))
#+nil
(defparameter *sun-direction* (unit-pitch-yaw (deg-rad 90) (deg-rad 0)))
#+nil
(defparameter *sky-color-foo* '(0.0 0.0 0.0))
#+nil
(defun neck-angle ()
  (/ (+ 1.0
	(-
	 (sb-cga:dot-product
	  (sb-cga:normalize (camera-matrix:camera-vec-forward *camera*))
	  (sb-cga:normalize *sun-direction*))))
     2.0))
   #+nil
   (mapcar 
    (lambda (a0 a1)
      (expt (alexandria:lerp (neck-angle) a0 a1) 0.5))
    *sky-color2*
    *sky-color*)
;;;;
#+nil
(defun select-block-with-scroll-wheel ()
  (setf *blockid*
	(let ((seq
	       #(3 13 12 24 1 2 18 17 20 5 89)))
	  (elt seq (mod (round window:*scroll-y*)
			(length seq))))))

;;FIXME -> select-block-with-scroll-wheel should use events instead?
#+nil
(select-block-with-scroll-wheel)
