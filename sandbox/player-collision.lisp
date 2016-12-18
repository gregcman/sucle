(in-package :sandbox)

(defun block-aabb ()
  (aabbcc::make-aabb
   :minx -0.5
   :miny -0.5
   :minz -0.5
   :maxx 0.5
   :maxy 0.5
   :maxz 0.5))

(defun player-aabb ()
  (aabbcc::make-aabb
   :minx -0.3
   :miny 0
   :minz -0.3
   :maxx 0.3
   :maxy 1.62
   :maxz 0.3))


(defparameter block-aabb (block-aabb))
(defparameter player-aabb (player-aabb))

(defun myafunc (px py pz vx vy vz)
  (let ((ourblocks (get-blocks-around-player px py pz)))
    (multiple-value-bind (minimum blocktouches) (blocktocontact ourblocks px py pz vx vy vz)
      (multiple-value-bind (xclamp yclamp zclamp)
	  (aabbcc::collapse-types blocktouches vx vy vz)
	(values minimum xclamp yclamp zclamp)))))

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
	  ablock
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
    (dotimes (x 3)
      (dotimes (y 4)
	(dotimes (z 3)
 	  (let ((blockx (round (- (+ x px) 1)))
		(blocky (round (- (ceiling (+ y py)) 1)))
		(blockz (round (- (+ z pz) 1))))
	    (let ((blockid (mat-pos (vector blockx blocky blockz))))
	      (if (eq t (aref mc-blocks::iscollidable blockid))
		  (push (vector blockx blocky blockz) places)))))))
    places))

