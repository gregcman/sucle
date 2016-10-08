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
  (let ((camera (getworld "player")))
    (mat:scale! cameraVelocity 0.9)
    (mat:add! (simplecam-pos camera) cameraVelocity)))

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
    (if t
	(setf daytime (/ (+ 1 (cos (/ (get-internal-run-time) (* 20 100)))) 2))
	(setf daytime 1))

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
    (progno
     (if isneaking
	 (mat:scale! delta 0.2))
     (if isprinting
	 (mat:scale! delta 1.3))
     (if (not onground)
	 (mat:scale! delta 0.2)))
    delta))

(defun key-jumps ()
  "keys for jumping"
  (let* ((delta (empty-vec4))
	 (lemod (good-func delta)))
    (if (or t
	    onground)
	(mapcar
	 lemod
	 `(("space" (0 ,(* 0.42 (expt tickscale 3)) 0 0))
	   ("lshift" (0 ,(* -0.42 (expt tickscale 3)) 0 0)))))
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
	(vox::setblock-with-update (+ a i -1) (- j 1) (+ b k -1) blockid)))))

(defun notaplatform (pos)
  (let ((i (elt pos 0))
	(j (elt pos 1))
	(k (elt pos 2)))
    (dotimes (a 3)
      (dotimes (b 3)
	(vox::setblock-with-update (+ a i -1) (+ j) (+ b k -1) 0)))))

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
  (vox::getblock
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
	  (let ((counter 0))
	    (dolist (n (sandbox::flat3-chunk light))
	      (vox::destroy-chunk-at x counter y)
	      (setf (gethash (vox::chunkhashfunc x counter y) vox::lighthash) n)
	      (incf counter)))
	  (let ((counter 0))
	    (dolist (n (sandbox::flat3-chunk skylight))
	      (vox::destroy-chunk-at x counter y)
	      (vox::setf (gethash (vox::chunkhashfunc x counter y) vox::skylighthash) n)
	      (incf counter)))
	  (let ((counter 0))
	    (dolist (n (sandbox::flat3-chunk blocks))
	      (vox::destroy-chunk-at x counter y)
	      (setf (gethash (vox::chunkhashfunc x counter y) vox::chunkhash) n)
	      (vox::dirtify x counter y)
	      (incf counter)))))))

(defun flat3-chunk (data)
  "takes flat chunk data and packs it into a chunk" 
  (let ((ourans nil))
    (dotimes (wow 8)
      (let ((new-chunk (vox::getachunk)))
	(dotimes (j 16)
	  (dotimes (i 16)
	    (dotimes (k 16)
	      (vox::set-chunk-block new-chunk i j k
			       (elt data (+ (* i 16 128) (+ j (* 16 wow)) (* k 128)))))))
	(push new-chunk ourans)))
    (nreverse ourans)))


(defparameter atest (if t
			cl-mc-shit::testchunk
			(byte-read #P "/home/imac/.minecraft/saves/New World/region/r.0.0.mcr")))

(defun helpchunk (x y)
   (let ((thechunk  (cl-mc-shit:mcr-chunk atest x y)))
     (if thechunk
	 (cl-mc-shit:chunk-data
	       thechunk)
	 nil)))

(defun expand-nibbles (vec)
  (let* ((len (length vec))
	 (newvec (make-array (* 2 len) :element-type '(unsigned-byte 4))))
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
