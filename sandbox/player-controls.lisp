(in-package :sandbox)

;;in this file
;;sprinting
;;sneaking
;;walking
;;collision with blocks
;;jumping
;;strafing
;;gravity
;;looking around
;;naturally this will be quite messy, as there are many facets
;;to player movement. before, the light code and importing and world
;; "world hash?!" were lumped with this for some reason-----

(defparameter lastw nil)
(defparameter wprev most-negative-fixnum)
(defparameter wpressprev nil)
(defparameter isprinting nil)
(defparameter isneaking nil)

(defun controls ()
  (let ((camera ourcam))
    (mat:add!
     cameraVelocity
     (keymovement camera))
    (progn
      (if (in:key-p :w)
	 ;;if it was pressed last round
	 (progn
	   (if (not wpressprev)
	       (progn
		 (if (> 150 (- (get-internal-run-time) wprev))
		     (setf isprinting t))))
	   (setf wpressprev t))
	 (progn
	   (setf isprinting nil)
	   (if wpressprev
	       (progn
		 (setf wprev (get-internal-run-time))
		 (setf wpressprev nil))))))
    
    (if (in:key-p :left-shift)
	(progn
	  (setf isprinting nil)
	  (setf isneaking t))
	(setf isneaking nil))

    (in:key-pressed-hook #\h (lambda () (someseq
			    (floor (row-major-aref (simplecam-pos camera) 0) 16)
			    (floor (row-major-aref (simplecam-pos camera) 2) 16))))

    (if (in:key-p :z)
	(%aplatform
	 (mat-world-pos (simplecam-pos camera))
	 3))
    (if (in:key-pressed-p :x)
	(progn
	  (notaplatform (mat-world-pos (simplecam-pos camera)))
	  (let ((wot (mat-world-pos (simplecam-pos camera))))
	    (incf (elt wot 1) 1)
	    (notaplatform wot))))
    (if (in:key-pressed-p :c) 
	(oneplatform
	 (mat-world-pos (simplecam-pos camera))
	 91))))

(defun mat-world-pos (mat)
  (vector
   (round (row-major-aref mat 0))
   (ceiling (row-major-aref mat 1))
   (round (row-major-aref mat 2))))

(defun mouse-looking (camera)
  (let* ((change (in:delta))
	 (x (* 1.25 1/360 (aref change 0)))
	 (y (* 1.25 1/360 (aref change 1))))
    (setf
     (simplecam-yaw camera)
     (mod (+ (simplecam-yaw camera) x) (* 2 pi)))
    (setf (simplecam-pitch camera)
	  (anothershit
	   (+ (simplecam-pitch camera) y) (/ pi 2)))))

(defun anothershit (x whatthefuck)
  "used to clamp the pitch"
  (if (> x whatthefuck)
      whatthefuck
      (if (< x (- whatthefuck))
	  (- whatthefuck)
	  x)))

(defun good-func (some-number)
  "maps keys to vectors"
  (lambda (x)
    (if (in::akeydown (first x))
	(mat:add! some-number
		  (mat:onebyfour (second x))))))

(defun empty-vec4 ()
  (mat:onebyfour '(0 0 0 0)))

(defun key-legs ()
  "keys for walking"
  (let* ((delta (empty-vec4))
	 (lemod (good-func delta)))
    (mapcar
     lemod
     '((:s ( 1  0  0  0))
       (:w (-1  0  0  0))
       (:a ( 0  0  1  0))
       (:d ( 0  0 -1  0))))
    (mat:scale! (mat:normalize! delta) (* 0.4 (expt tickscale 2)))
    (if isneaking
	(mat:scale! delta 0.2))
    (if isprinting
	(mat:scale! delta 1.3))
    (if (not onground)
	(mat:scale! delta 0.2))
    delta))

(defun key-jumps ()
  "keys for jumping"
  (let* ((delta (empty-vec4))
	 (lemod (good-func delta)))
    (if onground
	(mapcar
	 lemod
	 `((:space (0 ,(* 0.42 (expt tickscale 1)) 0 0)))))
    delta))

(defun keymovement (camera)
  "total keymovement"
  (mat:mmul! (mat:add (key-legs) (key-jumps))
	     (mat:rotation-matrix 0 1 0
				  (simplecam-yaw camera))))

(defun aplatform (pos blockid)
  (let ((i (elt pos 0))
	(j (elt pos 1))
	(k (elt pos 2)))
    (dotimes (a 3)
      (dotimes (b 3)
	(setblock-with-update (+ a i -1) (- j 1) (+ b k -1) blockid
				    (aref mc-blocks::lightvalue blockid))))))

(defun %aplatform (pos blockid)
  (let ((i (elt pos 0))
	(j (elt pos 1))
	(k (elt pos 2)))
    (setblock-with-update (+ i) (- j 1) (+ k) blockid
				(aref mc-blocks::lightvalue blockid))))

(defun oneplatform (pos blockid)
  (let ((i (elt pos 0))
	(j (elt pos 1))
	(k (elt pos 2)))
    (setblock-with-update (+ i) (- j 1) (+ k) blockid
				(aref mc-blocks::lightvalue blockid))))

(defun notaplatform (pos)
  (let ((i (elt pos 0))
	(j (elt pos 1))
	(k (elt pos 2)))
    (dotimes (a 3)
      (dotimes (b 3)
	(setblock-with-update (+ a i -1) (+ j) (+ b k -1) 0
				    0)))))


;;fuck me
;;this file contains::
;;loading nbt into the world
;;vector operations
;;player controls
;;player movement among blocks
;;this is not pretty
;;why did i name everything so retardedly?

(defparameter onground nil)
(defparameter cameraVelocity (mat:onebyfour '(0.0 0.0 0.0 0)))
(defparameter daytime 1.0)

(defun ease (x target fraction)
  (+ x (* fraction (- target x))))
;;70 is normal
;;110 is quake pro
(defparameter defaultfov 70)

(defun physinnit ()
  (sb-int:set-floating-point-modes :traps nil)
  (setf (simplecam-pos ourcam) (mat:onebyfour '(0 128 0 0)))
  (setf cameraVelocity (mat:onebyfour '(0 0 0 0)))
  (setf isprinting nil)
  (setf wprev most-negative-fixnum))

;;gravity is (* -0.08 (expt tickscale 2)) 0 0
;;falling friction is 0.98
;;0.6 * 0.91 is walking friction

(defun physics ()
  "a messy function for the bare bones physics"
  (setf daytime (case 10
		  (0 27.5069)
		  (2 9)
		  (9 0)
  		  (3 0)
		  (1 (/ (+ 0  (cos (/ (get-internal-real-time) 2000))) 0.5))
		  (10 1.0)))
  (let ((camera ourcam))
    (if isprinting
      (setf (simplecam-fov camera)
	    (ease (simplecam-fov camera) (* 1.15 defaultfov) 0.2))
      (setf (simplecam-fov camera)
	    (ease (simplecam-fov camera) defaultfov 0.2)))


    (let ((pos (mat-vec (simplecam-pos camera)))
	  (vel (mat-vec cameraVelocity)))
      (multiple-value-bind (px py pz ccx ccy ccz)
	  (aabbcc::step-motion #'myafunc
			       (elt pos 0)
			       (elt pos 1)
			       (elt pos 2)
			       (elt vel 0)
			       (elt vel 1)
			       (elt vel 2))
      ;;;;;WOWWOOWOWOWOWOW
	(if (and (>= 0 (elt vel 1)) ccy)
	    (setf onground t)
	    (setf onground nil))
	(setf (simplecam-pos camera) (vec-mat (vector px py pz)))
	(let ((foo (mat-vec cameravelocity)))
	  (setf cameraVelocity
		(vec-mat
		 (multiple-value-bind
		       (a b c) (aabbcc::clamp-vec (elt foo 0)
						  (elt foo 1)
						  (elt foo 2)
						  ccx ccy ccz)
		   (vector a b c))))))
      (let ((pos (mat-vec (simplecam-pos camera))))
	(let ((blocks-around (get-blocks-around-player (elt pos 0)
						       (elt pos 1)
						       (elt pos 2))))
	  (multiple-value-bind (x+ x- y+ y- z+ z-)
	      (block-touches blocks-around
			     (elt pos 0)
			     (elt pos 1)
			     (elt pos 2))
	    (setf onground y-)))))

    
    (if (not onground)
	(mat:add! cameraVelocity (mat:onebyfour (list 0 (* -0.08 (expt tickscale 2)) 0 0))))
    (let ((airscaled
	   (mat:onebyfour
	    (list
	     (row-major-aref cameraVelocity 0)
	     0
	     (row-major-aref cameraVelocity 2)
	     0))))
      (mat:scale! airscaled (* 0.9))
      (if onground (mat:scale! airscaled (* 0.6 0.91)))
      (if nil
	  (let ((speed (hypot (mat-lis airscaled))))
	    (print speed)
	    (if (> 0.05 speed)
		(setf isprinting nil))))
      (setf (row-major-aref cameraVelocity 0) (row-major-aref airscaled 0))
      (setf (row-major-aref cameraVelocity 2) (row-major-aref airscaled 2)))
    (setf (row-major-aref cameraVelocity 1)
	  (* (expt 0.98 tickscale)
	     (row-major-aref cameraVelocity 1)))
    (outofbounds camera)))

(defun look-around ()
  (mouse-looking ourcam))

(defun outofbounds (camera)
  (if (> -256 (row-major-aref (simplecam-pos camera) 1))
      (progn	  
	(setf (row-major-aref cameraVelocity 1) 0)
	(setf (row-major-aref (simplecam-pos camera) 1) 0)
	(setf (simplecam-pos camera) (mat:onebyfour (list 0 128 0 1))))))

(defun vec-mat (vec)
  (let ((newmat (mat:onebyfour '(0 0 0 0))))
    (dotimes (x (length vec))
      (setf (row-major-aref newmat x) (elt vec x)))
    newmat))

(defun mat-vec (mat)
  (vector
   (row-major-aref mat 0)
   (row-major-aref mat 1)
   (row-major-aref mat 2)))


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
	    (let ((blockid (round-pos blockx blocky blockz)))
	      (if (eq t (aref mc-blocks::iscollidable blockid))
		  (push (vector blockx blocky blockz) places)))))))
    places))

(defun round-pos (x y z)
  (world:getblock
   (round x)
   (round y)
   (round z)))

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
  (if (/= blockid (world:getblock i j k))
   (let ((old-light-value (world:getlight i j k)))
     (when (setf (world:getblock i j k) blockid)
       (if (< new-light-value old-light-value)
	   (progn
	     (de-light-node i j k)))
       (setf (world:getlight i j k) new-light-value)
       (sky-de-light-node i j k)
       (unless (zerop new-light-value)
	 (light-node i j k))
       (block-dirtify i j k)))))

(defun world-setup ()
  (clean-dirty)
  (world:setup-hashes))

(world-setup)
