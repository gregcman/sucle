(in-package :sandbox)

;;;;150 ms delay for sprinting
;;;;player eye height is 1.5, subtract 1/8 for sneaking

;;gravity is (* -0.08 (expt tickscale 2)) 0 0
;;falling friction is 0.98
;;0.6 * 0.91 is walking friction

;;fov::
;;70 is normal
;;110 is quake pro


(defparameter *yaw* nil)
(defparameter *pitch* nil)
(defparameter *xpos* nil)
(defparameter *ypos* nil)
(defparameter *zpos* nil)
(defparameter defaultfov 70)

(defparameter *xvel* 0)
(defparameter *yvel* 0)
(defparameter *zvel* 0)

(defparameter friction 0.9)

(defparameter noclip nil)

(setf *xpos* 0
      *ypos* 0
      *zpos* 0
      *yaw* 0
      *pitch* 0)

(defun controls ()
  (let ((speed 0.024))
    (let ((dir (complex 0 0)))
      (when (e:key-p :w)
	(incf dir #C(-1 0)))
      (when (e:key-p :a)
	(incf dir #C(0 1)))
      (when (e:key-p :s)
	(incf dir #C(1 0)))
      (when (e:key-p :d)
	(incf dir #C(0 -1)))
      (unless (zerop dir)
	(let ((rot-dir (* dir (cis *yaw*))))
	  (incf *xvel* (* speed (realpart rot-dir)))
	  (incf *zvel* (* speed (imagpart rot-dir))))))
    (when (e:key-p :space)
      (incf *yvel* speed))
    (when (e:key-p :left-shift)
      (decf *yvel* speed))))

(defparameter mouse-sensitivity (* 60.0 pi 1/180))

(defun mouse-looking ()
  (multiple-value-bind (dx dy) (delta)
    (let ((x (* mouse-sensitivity (/ dx 360.0)))
	  (y (* mouse-sensitivity (/ dy 360.0))))
      (multiple-value-bind (dyaw dpitch) (%sphere-mouse-help x y)
	(setf *yaw* (mod (+ *yaw* dyaw) (* 2 pi)))
	(setf *pitch* (clamp (+ *pitch* dpitch)
			     (* 0.99 (/ pi -2))
			     (* 0.99 (/ pi 2))))))))

(defparameter old-mouse-x 0)
(defparameter old-mouse-y 0)
(defun delta ()
  (multiple-value-bind (newx newy) (window:get-mouse-position)
    (multiple-value-prog1 (values
			   (- newx old-mouse-x)
			   (- newy old-mouse-y))
      (setf old-mouse-x newx
	    old-mouse-y newy))))


(defun %sphere-mouse-help (x y)
  (if (zerop x)
      (if (zerop y)
	  (values 0.0 0.0)
	  (values 0.0 y))
      (if (zerop y)
	  (values x 0.0)
	  (new-direction (coerce x 'single-float)
			 (coerce y 'single-float)))))


(defparameter mousecapturestate nil)
(defun remove-spurious-mouse-input ()
  (if (window:mice-locked-p)
      (case mousecapturestate
	((nil) 
	 (delta) ;;toss spurious mouse movement 
	 (setf mousecapturestate :justcaptured))
	(:justcaptured (setq mousecapturestate t))
	((t)))
      (setq mousecapturestate nil)))

(defun set-render-cam-pos (millisecs)
  (let ((multiplier (/ millisecs tick-delay)))
    (setf (camera-xpos *camera*) (+ *xpos* (* multiplier *xvel*))
	  (camera-ypos *camera*) (+ *ypos* (* multiplier *yvel*))
	  (camera-zpos *camera*) (+ *zpos* (* multiplier *zvel*))
	  (camera-pitch *camera*) *pitch*
	  (camera-yaw *camera*) *yaw*)))

(defun set-render-cam-look ()
  (setf (camera-pitch *camera*) *pitch*
	(camera-yaw *camera*) *yaw*))

(defparameter daytime 1.0)
(defparameter ticks/sec nil)
(defparameter tickscale nil)

(defun physinnit ()
  (setf ticks/sec 60.0)
  (setf tickscale (/ 20.0 ticks/sec))
  (setf tick-delay (/ 1000000.0 ticks/sec)))

(defun physics ()
  ;;escape to quitx
  (when (e:key-p :ESCAPE) (setq alivep nil))
  
  ;;e to escape mouse
  (when (e:key-p :E) (window:toggle-mouse-capture))
  (when (window:mice-locked-p)
    (controls)
    (when (e:key-p :r)
      (setblock-with-update (floor fistx)
			    (floor fisty)
			    (floor fistz)
			    49
			    0)))
  (setf *xvel* (* *xvel* friction))
  (setf *yvel* (* *yvel* friction))
  (setf *zvel* (* *zvel* friction))
  (collide-with-world)
  (let ((cos-pitch (cos *pitch*)))
    (let ((vx (- (* reach (* cos-pitch (cos *yaw*)))))
	  (vy (- (* reach (sin *pitch*))))
	  (vz (- (* reach (* cos-pitch (sin *yaw*))))))
      (multiple-value-bind (frac xclamp yclamp zclamp)
	  (punch-func (+ *xpos* -0.0) (+ *ypos* 0.0) (+ *zpos* -0.0) vx vy vz)
	(setf fistx (+ *xpos* (* frac vx))
	      fisty (+ *ypos* (* frac vy))
	      fistz (+ *zpos* (* frac vz)))))))

(defparameter fistx 0.0)
(defparameter fisty 0.0)
(defparameter fistz 0.0)

(defun collide-with-world ()
  (multiple-value-bind (new-x new-y new-z xclamp yclamp zclamp)
      (aabbcc::step-motion #'myafunc
			   *xpos* *ypos* *zpos*
			   *xvel* *yvel* *zvel*)
    (setf *xpos* new-x)
    (setf *ypos* new-y)
    (setf *zpos* new-z)
    (multiple-value-bind (x y z) (aabbcc::clamp-vec
				  *xvel* *yvel* *zvel*
				  xclamp yclamp zclamp)
      (setf *xvel* x)
      (setf *yvel* y)
      (setf *zvel* z))))

(defun shit (x)
  (print x global-output))
(defun look-around ()
  (mouse-looking))

(defun block-aabb ()
  (aabbcc::make-aabb
   :minx 0.0
   :miny 0.0
   :minz 0.0
   :maxx 1.0
   :maxy 1.0
   :maxz 1.0))

(defun player-aabb ()
  (aabbcc::make-aabb
   :minx -0.3
   :miny -1.5
   :minz -0.3
   :maxx 0.3
   :maxy 0.12
   :maxz 0.3))

(defun fist-aabb ()
  (aabbcc::make-aabb
   :minx -0.005
   :miny -0.005
   :minz -0.005
   :maxx 0.005
   :maxy 0.005
   :maxz 0.005))

(defparameter block-aabb (block-aabb))
(defparameter player-aabb (player-aabb))
(defparameter fist-aabb (fist-aabb))

(defparameter reach 5.0)

(defun fist-collide (blocklist px py pz dx dy dz)
  (let ((tot-min 1)
	(actual-contacts nil))
    (dolist (ablock blocklist)
      (multiple-value-bind (minimum type)
	  (aabbcc::aabb-collide
	   fist-aabb
	   px py pz
	   block-aabb
	   (elt ablock 0)
	   (elt ablock 1)
	   (elt ablock 2)
	   dx dy dz)
	(if (= minimum tot-min)
	    (push type actual-contacts)
	    (if (< minimum tot-min)
		(progn
		  (setq tot-min minimum)
		  (setq actual-contacts (list type)))))))
    (values
     tot-min
     actual-contacts)))

(defun punch-func (px py pz vx vy vz)
  (let ((ourblocks (get-blocks-around-player px py pz)))
    (multiple-value-bind (minimum blocktouches) (fist-collide ourblocks px py pz vx vy vz)
      (multiple-value-bind (xclamp yclamp zclamp)
	  (aabbcc::collapse-types blocktouches vx vy vz)
	(values minimum xclamp yclamp zclamp)))))

(defun myafunc (px py pz vx vy vz)
  (let ((ourblocks (unless noclip (get-blocks-around-player px py pz))))
    (multiple-value-bind (minimum blocktouches) (blocktocontact ourblocks px py pz vx vy vz)
      (multiple-value-bind (xclamp yclamp zclamp)
	  (aabbcc::collapse-types blocktouches vx vy vz)
	(values minimum xclamp yclamp zclamp)))))

(defun goto (x y z)
  (setf *xpos* x
	*ypos* y
	*zpos* z))

(defun blocktocontact (blocklist px py pz vx vy vz)
  "for a list of blocks, a position, and velocity, 
collect all the nearest collisions with the player"
  (let ((tot-min 1)
	(actual-contacts nil))
    (dolist (ablock blocklist)
      (multiple-value-bind (minimum type)
	  (aabbcc::aabb-collide
	   player-aabb
	   px py pz
	   block-aabb
	   (elt ablock 0)
	   (elt ablock 1)
	   (elt ablock 2)
	   vx vy vz)
	(if (= minimum tot-min)
	    (push type actual-contacts)
	    (if (< minimum tot-min)
		(progn
		  (setq tot-min minimum)
		  (setq actual-contacts (list type)))))))
    (values
     tot-min
     actual-contacts)))

(defun block-touches (blocklist px py pz)
  "return a list of which sides are touching a block"
  (let ((x+ nil)
	(x- nil)
	(y+ nil)
	(y- nil)
	(z+ nil)
	(z- nil))
    (dolist (theplace blocklist)
      (multiple-value-bind (bx sx by sy bz sz) ;;b = big =positive s = small = negative
	  (aabbcc::aabb-contact px py pz
				player-aabb
				(elt theplace 0)
				(elt theplace 1)
				(elt theplace 2)
				block-aabb)
	(if bx (setq x+ t))
	(if sx (setq x- t))
	(if by (setq y+ t))
	(if sy (setq y- t))
	(if bz (setq z+ t))
	(if sz (setq z- t))))
    (values x+ x- y+ y- z+ z-)))

(defun get-blocks-around-player (px py pz)
  "get the blocks around player"
  (let ((places nil))
    (dotimes (x 5)
      (dotimes (y 4)
	(dotimes (z 5)
 	  (let ((blockx (floor (- (+ x (truncate px)) 2)))
		(blocky (floor (- (+ y (truncate py)) 2)))
		(blockz (floor (- (+ z (truncate pz)) 2))))
	    (let ((blockid (world:getblock blockx blocky blockz)))
	      (if (eq t (aref mc-blocks::iscollidable blockid))
		  (push (vector
			 blockx
			 blocky
			 blockz) places)))))))
    places))

;;dirty chunks is a list of modified chunks
;;we do not want anyone to see a raw list!
(defparameter dirtychunks nil)
(defun clean-dirty ()
  (setf dirtychunks (q::make-uniq-q)))
(defun dirty-pop ()
  (q::uniq-pop dirtychunks))
(defun dirty-push (item)
  (q::uniq-push item dirtychunks))
(defun block-dirtify (i j k)
  (dirty-push (world:chop (world:chunkhashfunc i k j))))

(defun setblock-with-update (i j k blockid new-light-value)
  (let ((old-blockid (world:getblock i j k)))
    (if (/= blockid old-blockid)
	(let ((old-light-value (world:getlight i j k)))
	  (when (setf (world:getblock i j k) blockid)
	    (when (< new-light-value old-light-value)  
	      (de-light-node i j k))
	    (setf (world:getlight i j k) new-light-value)
	    (sky-de-light-node i j k)
	    (unless (zerop new-light-value)
	      (light-node i j k))
	    (flet ((check (a b c)
		     (light-node (+ i a) (+ j b) (+ k c))
		     (sky-light-node (+ i a) (+ j b) (+ k c))))
	      (check -1 0 0)
	      (check 1 0 0)
	      (check 0 -1 0)
	      (check 0 1 0)
	      (check 0 0 -1)
	      (check 0 0 1))
	    (block-dirtify i j k))))))

(defun plain-setblock (i j k blockid new-light-value)
  (let ((old-blockid (world:getblock i j k)))
    (if (/= blockid old-blockid)
	(when (setf (world:getblock i j k) blockid)
	  (setf (world:getlight i j k) new-light-value)))))

