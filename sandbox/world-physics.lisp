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

(defun ease (x target fraction)
  (+ x (* fraction (- target x))))
;;70 is normal
;;110 is quake pro
(defparameter defaultfov 70)

(defun physinnit ()
  (sb-int:set-floating-point-modes :traps nil)
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
  (setf daytime (case 1
		  (0 27.5069)
		  (2 9)
		  (9 0)
		  (3 0)
		  (1 (/ (+ 0  (cos (/ (get-internal-real-time) 2000))) 0.5))
		  (10 1.0)))
  (let ((camera (getworld "player")))
    (if isprinting
      (setf (simplecam-fov camera)
	    (ease (simplecam-fov camera) (* 1.15 defaultfov) 0.2))
      (setf (simplecam-fov camera)
	    (ease (simplecam-fov camera) defaultfov 0.2)))
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

    (if (in:key-pressed-p :z)
	(aplatform
	 (mat-world-pos (simplecam-pos camera))
	 2))
    (if (in:key-pressed-p :x)
	(progn
	  (notaplatform (mat-world-pos (simplecam-pos camera)))
	  (notaplatform (vecadd (mat-world-pos (simplecam-pos camera)) (vector 0 1 0)))))
    (if (in:key-pressed-p :c) 
	(oneplatform
	 (mat-world-pos (simplecam-pos camera))
	 91))))

(defun mat-world-pos (mat)
  (vector
   (round (row-major-aref mat 0))
   (floor (row-major-aref mat 1))
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
    (if	onground
	(mapcar
	 lemod
	 `((:space (0 ,(* 0.42 (expt tickscale 1)) 0 0)))))
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

(defun oneplatform (pos blockid)
  (let ((i (elt pos 0))
	(j (elt pos 1))
	(k (elt pos 2)))
    (setblock-with-update (+ i) (- j 1) (+ k) blockid)))

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
	      (skylight (getskylightlizz thechunk))
	      (meta (getmetadatalizz thechunk))
	      (leheight (getheightlizz thechunk)))
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
	     meta
	     (lambda (x y z b)
	       (setmeta x y z b))
	     xscaled 0 yscaled)
	    (sandbox::flat2-chunk
	     leheight
	     (lambda (x y b)
	       (setheight x y b))
	     xscaled yscaled)
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

(defun flat2-chunk (data setfunc xoffset yoffset)
  (dotimes (j 16)
    (dotimes (i 16)
      (funcall setfunc (+ xoffset i) (+ yoffset j)
	       (elt data (+ i (+ (* 16 j))))))))


(progno
  (defparameter atest (if nil
			  cl-mc-shit::testchunk
			  (byte-read #P "/home/imac/.minecraft/saves/New World/region/r.0.1.mcr"))))

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

(defun nbt-open (lizz)
  (third
   (first
    (third
     lizz))))

(defun gettag (lestring lizz)
  (dolist (tag lizz)
    (if (equal lestring (second tag))
	(return-from gettag (third tag)))))

(defun getmetadatalizz (lizz)
  (expand-nibbles
   (gettag "Data"
	   (nbt-open lizz))))

(defun getskylightlizz (lizz)
  (expand-nibbles
   (gettag "SkyLight"
	   (nbt-open lizz))))

(defun getlightlizz (lizz)
  (expand-nibbles
   (gettag "BlockLight"
	   (nbt-open lizz))) )

(defun getblockslizz (lizz)
  (gettag
   "Blocks"
   (nbt-open lizz)))

(defun getheightlizz (lizz)
  (gettag
   "HeightMap"
   (nbt-open lizz)))

(defun genhash ()
  (make-hash-table :test (function eql)))

;;chunkhash stores all of the chunks in a hasmap.
;;chunks accessed by '(x y z) in chunk coords
(defparameter chunkhash (genhash))
(defparameter lighthash (genhash))
(defparameter skylighthash (genhash))
(defparameter metahash (genhash))

(defparameter heighthash (genhash))

;;dirty chunks is a list of modified chunks 
(defparameter dirtychunks nil)

(defun clearworld ()
  (vox::send-to-free-mem chunkhash)
  (vox::send-to-free-mem lighthash)
  (vox::send-to-free-mem skylighthash)
  (vox::send-to-free-mem metahash)
  (pix::send-to-free-mem heighthash)
  (setf dirtychunks nil))

(setf (fdefinition 'getblock) (vox::func-get chunkhash 0))
(setf (fdefinition 'setblock) (vox::func-set chunkhash 0))
(defun (setf getblock) (new x y z)
    (setblock x y z new))

(setf (fdefinition 'getlight) (vox::func-get lighthash 0))
(setf (fdefinition 'setlight) (vox::func-set lighthash 0))
(defun (setf getlight) (new x y z)
    (setlight x y z new))

(setf (fdefinition 'skygetlight) (vox::func-get skylighthash 15))
(setf (fdefinition 'skysetlight) (vox::func-set skylighthash 15))
(defun (setf skygetlight) (new x y z)
    (skysetlight x y z new))

(setf (fdefinition 'getmeta) (vox::func-get metahash 0))
(setf (fdefinition 'setmeta) (vox::func-set metahash 0))
(defun (setf getmeta) (new x y z)
    (setmeta x y z new))

(setf (fdefinition 'getheight) (pix::func-get heighthash 0))
(setf (fdefinition 'setheight) (pix::func-set heighthash 0))
(defun (setf getheight) (new x y)
    (setheight x y new))

(defun block-dirtify (i j k)
  (pushnew (list (ash i -4) (ash j -4) (ash k -4)) dirtychunks :test 'equal))

(defun dirtify (x y z)
  (pushnew (list x y z) dirtychunks :test 'equal))

(defun update-height (x y)
  (block wow
    (dorange (z 0 255)
	     (let ((val (- 255 z)))
	       (let ((the-block (getblock x val y)))
		 (let ((ans 
			 (eq t 
			      (aref mc-blocks::opaquecubelooukup the-block))))
		   (if ans
		       (return-from wow
			 (setf (getheight x y) val)))))))
    (setheight x y 0)))

(defun isOpaque (id)
  (eq t (aref mc-blocks::opaquecubelooukup id)))

(defun lightnode (ans)
  (if ans
      (progn
	(let* ((ourpos (pop ans))
	       (i (first ourpos))
	       (j (second ourpos))
	       (k (third ourpos)))
	  (let ((courant (apply #'getlight ourpos)))
	    (unless (zerop courant)
	      (let ((current (1- courant)))
		(flet ((settest (x y z)
			 (unless (isOpaque (getblock x y z))
			   (setf (getlight x y z) current)
			   (pushnew (list x y z) ans :test #'equal))))
		  (let ((i- (getlight (- i 1) (+ j 0) (+ k 0)))
			(i+ (getlight (+ i 1) (+ j 0) (+ k 0)))
			(j- (getlight (+ i 0) (- j 1) (+ k 0)))
			(j+ (getlight (+ i 0) (+ j 1) (+ k 0)))
			(k- (getlight (+ i 0) (+ j 0) (- k 1)))
			(k+ (getlight (+ i 0) (+ j 0) (+ k 1))))
		    (if (< i- current)
			(settest (- i 1) (+ j 0) (+ k 0)))
		    (if (< i+ current)
			(settest (+ i 1) (+ j 0) (+ k 0)))
		    (if (< j- current)
			(settest (+ i 0) (- j 1) (+ k 0)))
		    (if (< j+ current)
			(settest (+ i 0) (+ j 1) (+ k 0)))
		    (if (< k- current)
			(settest (+ i 0) (+ j 0) (- k 1)))
		    (if (< k+ current)
			(settest (+ i 0) (+ j 0) (+ k 1)))))))))
	(lightnode ans))))

;;first we remove all the possible lights that could be affected by
;;the light we want to remove, then the surrounding lights fill in the
;;holes.
(defun delightnode (ans other)
  (if ans
      (progn
	(let* ((ourpos (pop ans))
	       (i (first ourpos))
	       (j (second ourpos))
	       (k (third ourpos)))
	  (let ((current (getlight i j k)))
	    (setf (getlight i j k) 0)
	    (flet ((settest (x y z)
		     (unless (isopaque (getblock x y z))
		       (pushnew (list x y z) ans :test #'equal)))
		   (lightprop (x y z)
		     (pushnew (list x y z) other :test #'equal)))
	      (let ((i- (getlight (- i 1) (+ j 0) (+ k 0)))
		    (i+ (getlight (+ i 1) (+ j 0) (+ k 0)))
		    (j- (getlight (+ i 0) (- j 1) (+ k 0)))
		    (j+ (getlight (+ i 0) (+ j 1) (+ k 0)))
		    (k- (getlight (+ i 0) (+ j 0) (- k 1)))
		    (k+ (getlight (+ i 0) (+ j 0) (+ k 1))))
		(unless (zerop i-)
		  (if (< i- current)
		      (settest (+ i -1) (+ j 0) (+ k 0))   
		      (lightprop (+ i -1) (+ j 0) (+ k 0))))
		(unless (zerop i+)
		  (if (< i+ current)
		      (settest (+ i 1) (+ j 0) (+ k 0))   
		      (lightprop (+ i 1) (+ j 0) (+ k 0))))
		(unless (zerop j-)
		  (if (< j- current)
		      (settest (+ i 0) (+ j -1) (+ k 0))   
		      (lightprop (+ i 0) (+ j -1) (+ k 0))))
		(unless (zerop j+)
		  (if (< j+ current)
		      (settest (+ i 0) (+ j 1) (+ k 0))   
		      (lightprop (+ i 0) (+ j 1) (+ k 0))))
		(unless (zerop k-)
		  (if (< k- current)
		      (settest (+ i 0) (+ j 0) (+ k -1))   
		      (lightprop (+ i 0) (+ j 0) (+ k -1))))
		(unless (zerop k+)
		  (if (< k+ current)
		      (settest (+ i 0) (+ j 0) (+ k 1))   
		      (lightprop (+ i 0) (+ j 0) (+ k 1))))))))
	(delightnode ans other))
      (if other
	  (lightnode other))))

(defun skylightnode (ans)
  (if ans
      (progn
	(let* ((ourpos (pop ans))
	       (i (first ourpos))
	       (j (second ourpos))
	       (k (third ourpos)))
	  (let* ((current (apply #'skygetlight ourpos))
		 (1-current (1- current)))
	    (flet ((settest (x y z)
		     (unless (isOpaque (getblock x y z))
		       (setf (skygetlight x y z) (1- current))
		       (pushnew (list x y z) ans :test #'equal)))
		   (wowtest (x y z)
		     (unless (or (isOpaque (getblock x y z))
				 (> 0 y))
		       (setf (skygetlight x y z) 15)
		       (pushnew (list x y z) ans :test #'equal))))
	      (let ((i- (skygetlight (- i 1) (+ j 0) (+ k 0)))
		    (i+ (skygetlight (+ i 1) (+ j 0) (+ k 0)))
		    (j- (skygetlight (+ i 0) (- j 1) (+ k 0)))
		    (j+ (skygetlight (+ i 0) (+ j 1) (+ k 0)))
		    (k- (skygetlight (+ i 0) (+ j 0) (- k 1)))
		    (k+ (skygetlight (+ i 0) (+ j 0) (+ k 1))))
		(if (< i- 1-current)
		    (settest (- i 1) (+ j 0) (+ k 0)))
		(if (< i+ 1-current)
		    (settest (+ i 1) (+ j 0) (+ k 0)))
		(if (= 15 current)
		    (wowtest (+ i 0) (- j 1) (+ k 0))
		    (if (< j- 1-current)
			(settest (+ i 0) (- j 1) (+ k 0))))
		(if (< j+ 1-current)
		    (settest (+ i 0) (+ j 1) (+ k 0)))
		(if (< k- 1-current)
		    (settest (+ i 0) (+ j 0) (- k 1)))
		(if (< k+ 1-current)
		    (settest (+ i 0) (+ j 0) (+ k 1)))))))
	(skylightnode ans))))

;;first we remove all the possible lights that could be affected by
;;the light we want to remove, then the surrounding lights fill in the
;;holes.
(defun skydelightnode (ans other)
  (if ans
      (progn
	(let* ((ourpos (pop ans))
	       (i (first ourpos))
	       (j (second ourpos))
	       (k (third ourpos)))
	  (let ((current (skygetlight i j k)))
	    (setf (skygetlight i j k) 0)
	    (flet ((settest (x y z)
		     (unless (or (isopaque (getblock x y z))
				 (< y 0))
		       (pushnew (list x y z) ans :test #'equal)))
		   (lightprop (x y z)
		     (pushnew (list x y z) other :test #'equal)))
	      (let ((i- (skygetlight (- i 1) (+ j 0) (+ k 0)))
		    (i+ (skygetlight (+ i 1) (+ j 0) (+ k 0)))
		    (j- (skygetlight (+ i 0) (- j 1) (+ k 0)))
		    (j+ (skygetlight (+ i 0) (+ j 1) (+ k 0)))
		    (k- (skygetlight (+ i 0) (+ j 0) (- k 1)))
		    (k+ (skygetlight (+ i 0) (+ j 0) (+ k 1))))
		(unless (zerop i-)
		  (if (< i- current)
		      (settest (+ i -1) (+ j 0) (+ k 0))   
		      (lightprop (+ i -1) (+ j 0) (+ k 0))))
		(unless (zerop i+)
		  (if (< i+ current)
		      (settest (+ i 1) (+ j 0) (+ k 0))   
		      (lightprop (+ i 1) (+ j 0) (+ k 0))))
		(unless (zerop j-)
		  (if (< j- current)
		      (settest (+ i 0) (+ j -1) (+ k 0))   
		      (lightprop (+ i 0) (+ j -1) (+ k 0))))
		(if (= 15 current)
		    (settest (+ i 0) (+ j -1) (+ k 0)))
		(unless (zerop j+)
		  (if (< j+ current)
		      (settest (+ i 0) (+ j 1) (+ k 0))   
		      (lightprop (+ i 0) (+ j 1) (+ k 0))))
		(unless (zerop k-)
		  (if (< k- current)
		      (settest (+ i 0) (+ j 0) (+ k -1))   
		      (lightprop (+ i 0) (+ j 0) (+ k -1))))
		(unless (zerop k+)
		  (if (< k+ current)
		      (settest (+ i 0) (+ j 0) (+ k 1))   
		      (lightprop (+ i 0) (+ j 0) (+ k 1))))))))
	(skydelightnode ans other))
      (if other
	  (skylightnode other))))

(defun setblock-with-update (i j k blockid)
  (delightnode (list (list i j k)) nil)
  (setlight i j k (aref mc-blocks::lightvalue blockid))
  (skydelightnode (list (list i j k)) nil)
  (lightnode (list (list i j k)))
  (if (setblock i j k blockid)
      (block-dirtify i j k)))
