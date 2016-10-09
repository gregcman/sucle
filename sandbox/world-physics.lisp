(in-package :sandbox)

(defparameter onground nil)
(defparameter cameraVelocity (mat:onebyfour '(0.0 0.0 0.0 0)))
(defparameter worldhash (make-hash-table :test #'equal))
(defparameter isneaking nil)

(defun getworld (name)
  (gethash name worldhash))

(defun setworld (name newval)
  (setf (gethash name worldhash) newval))

(defparameter daytime 1.0)
(defparameter ourcam (make-simplecam))

(defun physinnit ()
  (setf (simplecam-pos ourcam) (mat:onebyfour '(0 128 0 0)))
  (setf cameraVelocity (mat:onebyfour '(0 0 0 0)))
  (setworld "player" ourcam)
  (setf isprinting nil)
  (setf wprev most-negative-fixnum))

;;gravity is (* -0.08 (expt tickscale 2)) 0 0
;;falling friction is 0.98
;;0.6 * 0.91 is walking friction
(defun physics ()
  "a messy function for the bare bones physics"
  (let ((camera (getworld "player")))
    (let ((wowzer nil)
	  (collisiondata nil)
	  (velclamp nil))
      (multiple-value-bind (a b c)
	  (aabbcc::finish-clamps #'myafunc
				 (mat-vec (simplecam-pos camera))
				 (mat-vec cameraVelocity))
	(setf wowzer a)
	(setf collisiondata b)
	(setf velclamp c))
      (setf onground (if (numberp (elt collisiondata 2))
			 (= 0 (elt collisiondata 2))
			 (eq :contact (elt collisiondata 2))))

      (setf (simplecam-pos camera)
	    (vec-mat
	     wowzer))
      (setf cameraVelocity
	    (mat-clamper cameraVelocity velclamp))

      
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
      (outofbounds camera))))

(defun outofbounds (camera)
  (if (> 0 (row-major-aref (simplecam-pos camera) 1))
      (progn	  
	(setf (row-major-aref cameraVelocity 1) 0)
	(setf (row-major-aref (simplecam-pos camera) 1) 0)
	(setf (simplecam-pos camera) (mat:onebyfour (list 0 128 0 1))))))

(defparameter lastw nil)
(defparameter wprev most-negative-fixnum)
(defparameter wpressprev nil)
(defparameter isprinting nil)

(defmacro toggle (var)
  `(setf ,var (not ,var)))

(defun controls ()
  (let ((camera (getworld "player")))
    (mouse-looking camera)
    (mat:add!
     cameraVelocity
     (keymovement camera))
    (progn
     (if (in::akeydown "w")
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
    
    (if (in::akeydown "lshift")
	(progn
	  (setf isprinting nil)
	  (setf isneaking t))
	(setf isneaking nil))

    (in:p+1 #\h (lambda () (someseq
			    (floor (row-major-aref (simplecam-pos camera) 0) 16)
			    (floor (row-major-aref (simplecam-pos camera) 2) 16))))

    (in:p0 3 (lambda () (aplatform
			  (mat-world-pos (simplecam-pos camera))
			  2)))
    (in:p0 2 (lambda ()
		(notaplatform (mat-world-pos (simplecam-pos camera)))
		(notaplatform (vecadd (mat-world-pos (simplecam-pos camera)) (vector 0 1 0)))))))

(defun mat-world-pos (mat)
  (vector
   (round (row-major-aref mat 0))
   (round (row-major-aref mat 1))
   (round (row-major-aref mat 2))))

(defun mouse-looking (camera)
  (let* ((change (in:delta))
	 (x (* 1/360 (aref change 0)))
	 (y (* 1/360 (aref change 1))))
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
     '(("s" ( 1  0  0  0))
       ("w" (-1  0  0  0))
       ("a" ( 0  0  1  0))
       ("d" ( 0  0 -1  0))))
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
    (if	onground
	(mapcar
	 lemod
	 `(("space" (0 ,(* 0.42 (expt tickscale 1)) 0 0)))))
    delta))

(defun keymovement (camera)
  "total keymovement"
  (mat:mmul! (mat:add (key-legs) (key-jumps))
	     (mat:rotation-matrix 0 1 0
				  (simplecam-yaw camera))))

(defun blocktocontact (blocklist vec3player vel)
  (mapcar
   (lambda (theplace)
     (aabbcc::%aabb-intersect
      (aabbcc::player-aabb)
      vec3player
      (aabbcc::block-aabb)
      theplace
      vel))
   blocklist))

(defun get-blocks-around-player (vec3player vel)
  (declare (ignore vel))
  (let ((places nil))
    (dotimes (x 3)
      (dotimes (y 4)
	(dotimes (z 3)
	  (let ((blockx (round (- (+ x (elt vec3player 0)) 1)))
		(blocky (round (- (ceiling (+ y (elt vec3player 1))) 1)))
		(blockz (round (- (+ z (elt vec3player 2)) 1))))
	    (let ((blockid (mat-pos (vector blockx blocky blockz))))
	      (if (eq t (aref mc-blocks::iscollidable blockid))
		  (push (vector blockx blocky blockz) places)))))))
    places))

(defun myafunc (vec3player vel)
  (let ((ourblocks (get-blocks-around-player vec3player vel)))
    (let ((ourcontacts (blocktocontact ourblocks vec3player vel)))
      (let ((totcollisoins (aabbcc::collapsecollisions ourcontacts)))
        totcollisoins))))

(defun insert-at (num vec place)
  (let* ((start (subseq vec 0 place))
	 (end (subseq vec place (length vec))))
    (concatenate 'vector start (vector num) end)))

(defun delete-at (vec place)
  (let* ((start (subseq vec 0 place))
	 (end (subseq vec (1+ place) (length vec))))
    (concatenate 'vector start end)))

(defun aplatform (pos blockid)
  (let ((i (elt pos 0))
	(j (elt pos 1))
	(k (elt pos 2)))
    (dotimes (a 3)
      (dotimes (b 3)
	(setblock-with-update (+ a i -1) (- j 1) (+ b k -1) blockid)))))

(defun notaplatform (pos)
  (let ((i (elt pos 0))
	(j (elt pos 1))
	(k (elt pos 2)))
    (dotimes (a 3)
      (dotimes (b 3)
	(setblock-with-update (+ a i -1) (+ j) (+ b k -1) 0)))))

(defun vec-mat (vec)
  (let ((newmat (mat:onebyfour '(0 0 0 0))))
    (dotimes (x (length vec))
      (setf (row-major-aref newmat x) (elt vec x)))
    newmat))

(defun mat-clamper (mat coldata)
  (vec-mat
   (aabbcc::clamp-vec (mat-vec mat) coldata)))

(defun mat-vec (mat)
  (vector
   (row-major-aref mat 0)
   (row-major-aref mat 1)
   (row-major-aref mat 2)))

(defun mat-pos (mat)
  (getblock
   (round (row-major-aref mat 0))
   (round (row-major-aref mat 1))
   (round (row-major-aref mat 2))))

(defun vecscale (vec3 scale)
  (vector
   (* scale (elt vec3 0))
   (* scale (elt vec3 1))
   (* scale (elt vec3 2))))

(defun vecsubtract (vec3 vec32)
  (vector
   (- (elt vec3 0) (elt vec32 0))
   (- (elt vec3 1) (elt vec32 1))
   (- (elt vec3 2) (elt vec32 2))))

(defun vecadd (vec3 vec32)
  (vector
   (+ (elt vec3 0) (elt vec32 0))
   (+ (elt vec3 1) (elt vec32 1))
   (+ (elt vec3 2) (elt vec32 2))))

(defun veczerop (vec)
  (dotimes (n (length vec))
    (if (zerop (elt vec n))
	nil
	(return-from veczerop nil)))
  t)

(defun someseq (x y)
  (let* ((thechunk (helpchunk x y)))
    (if thechunk
	(let ((light (getlightlizz thechunk))
	      (blocks (getblockslizz thechunk))
	      (skylight (getskylightlizz thechunk)))
	  (let ((xscaled (ash x 4))
		(yscaled (ash y 4)))
	    (sandbox::flat3-chunk
	     light
	     (lambda (x y z b)
	       (setlight x y z b))
	     xscaled 0 yscaled)
	    (sandbox::flat3-chunk
	     skylight
	     (lambda (x y z b)
	       (skysetlight x y z b))
	     xscaled 0 yscaled)
	    (sandbox::flat3-chunk
	     blocks
	     (lambda (x y z b)
	       (setblock x y z b))
	     xscaled 0 yscaled))))))

(defun flat3-chunk (data setfunc xoffset yoffset zoffset)
  (dotimes (wow 8)
    (dotimes (j 16)
      (dotimes (i 16)
	(dotimes (k 16)
	  (funcall setfunc (+ xoffset i) (+ yoffset (* 16 wow) j) (+ zoffset k)
		   (elt data (+ (* i 16 128) (+ j (* 16 wow)) (* k 128)))))))))


(defparameter atest (if nil
			cl-mc-shit::testchunk
			(byte-read #P "/home/imac/.minecraft/saves/New World/region/r.0.1.mcr")))

(defun helpchunk (x y)
   (let ((thechunk  (cl-mc-shit:mcr-chunk atest x y)))
     (if thechunk
	 (cl-mc-shit:chunk-data
	       thechunk)
	 nil)))

(defun expand-nibbles (vec)
  (let* ((len (length vec))
	 (newvec (make-array (* 2 len) :element-type '(unsigned-byte 8))))
    (dotimes (x len)
      (multiple-value-bind (a b) (floor (aref vec x) 16)
	(setf (aref newvec (* 2 x)) b)
	(setf (aref newvec (1+ (* 2 x))) a)))
    newvec))

(defun getskylightlizz (lizz)
  (expand-nibbles
   (gettag "SkyLight"
	   (third
	    (first
	     (third
	      lizz))))) )

(defun getlightlizz (lizz)
  (expand-nibbles
   (gettag "BlockLight"
	   (third
	    (first
	     (third
	      lizz))))) )

(defun gettag (lestring lizz)
  (dolist (tag lizz)
    (if (equal lestring (second tag))
	(return-from gettag (third tag)))))

(defun getblockslizz (lizz)
  (gettag
   "Blocks"
   (third
    (first
     (third
      lizz)))))


;;chunkhash stores all of the chunks in a hasmap.
;;chunks accessed by '(x y z) in chunk coords
(defparameter chunkhash (make-hash-table :test (function eql)))
(defparameter lighthash (make-hash-table :test (function eql)))
(defparameter skylighthash (make-hash-table :test (function eql)))
;;dirty chunks is a list of modified chunks 
(defparameter dirtychunks nil)

(defun clearworld ()
  (send-to-free-mem chunkhash)
  (send-to-free-mem lighthash)
  (send-to-free-mem skylighthash)
  (setf dirtychunks nil))

(setf (fdefinition 'getblock) (vox::func-get chunkhash 0))
(setf (fdefinition 'getlight) (vox::func-get lighthash 15))
(setf (fdefinition 'skygetlight) (vox::func-get skylighthash 15))
(setf (fdefinition 'setblock) (vox::func-set chunkhash))
(setf (fdefinition 'setblock) (vox::func-set chunkhash))
(setf (fdefinition 'setlight) (vox::func-set lighthash))
(setf (fdefinition 'skysetlight) (vox::func-set skylighthash))

(defun block-dirtify (i j k)
  (pushnew (list (ash i -4) (ash j -4) (ash k -4)) dirtychunks :test 'equal))

(defun dirtify (x y z)
  (pushnew (list x y z) dirtychunks :test 'equal))

(defun setblock-with-update (i j k blockid)
  (if (setblock i j k blockid)
      (block-dirtify i j k)))
