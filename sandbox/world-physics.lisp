(in-package :sandbox)

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
(defparameter isneaking nil)
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
  (if (> 0 (row-major-aref (simplecam-pos camera) 1))
      (progn	  
	(setf (row-major-aref cameraVelocity 1) 0)
	(setf (row-major-aref (simplecam-pos camera) 1) 0)
	(setf (simplecam-pos camera) (mat:onebyfour (list 0 128 0 1))))))

(defmacro toggle (var)
  `(setf ,var (not ,var)))

(defun mat-world-pos (mat)
  (vector
   (round (row-major-aref mat 0))
   (floor (row-major-aref mat 1))
   (round (row-major-aref mat 2))))


(defun empty-vec4 ()
  (mat:onebyfour '(0 0 0 0)))

;;;Why are theses functions here?

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

(defparameter atest cl-mc-shit::testchunk)

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



(defun setblock-with-update (i j k blockid)
  (delightnode (list (list i j k)) nil)
  (setlight i j k (aref mc-blocks::lightvalue blockid))
  (skydelightnode (list (list i j k)) nil)
  (lightnode (list (list i j k)))
  (if (setblock i j k blockid)
      (block-dirtify i j k)))
