(in-package :sandbox)

;;chunkhash stores all of the chunks in a hasmap.
;;chunks accessed by '(x y z) in chunk coords
(defparameter chunkhash (make-hash-table :test (function eql)))
(defparameter lighthash (make-hash-table :test (function eql)))
(defparameter skylighthash (make-hash-table :test (function eql)))
;;dirty chunks is a list of modified chunks 
(defparameter dirtychunks nil)

(defun clearworld ()
  (send-to-free-mem chunkhash)
  (clrhash chunkhash)
  (send-to-free-mem lighthash)
  (clrhash lighthash)
  (send-to-free-mem skylighthash)
  (clrhash skylighthash)
  (setf dirtychunks nil))

(defun send-to-free-mem (hash)
  (maphash
   (lambda (k v)
     (declare (ignore k))
     (free-chunk v))
   hash))

(defparameter freechunkmempool nil)

(defun free-chunk (lechunk)
  (push lechunk freechunkmempool))

(defun empty-chunk ()
  "makes an empty chunk"
  (make-array (* 16 16 16) :element-type '(unsigned-byte 8)))

(defun getachunk ()
  (let ((somechunk (pop freechunkmempool)))
    (if somechunk
	somechunk
	(empty-chunk))))

(defun clearchunk (achunk)
  (nsubst-if-not 0 #'zerop achunk))

(defun plus2^19 (n)
  (declare (type fixnum n))
  (+ n (ash 1 19)))

(defun minus2^19 (n)
  (declare (type fixnum n))
  (- n (ash 1 19)))

(defun chunkhashfunc (x y z)
  (declare (type fixnum x y z))
  (+ (plus2^19 z) (ash (+ (plus2^19 y) (ash (plus2^19 x) 20)) 20)))

(defun unchunkhashfunc (ah)
  (declare (type fixnum ah))
  (let* ((z (logand ah (1- (ash 1 20))))
	 (xy (ash ah -20))
	 (y (logand xy (1- (ash 1 20))))
	 (x (ash xy -20)))
    (list (minus2^19 x) (minus2^19 y) (minus2^19 z))))

(defun getchunkat (hash x y z)
  (gethash (chunkhashfunc x y z) hash))

(defun setchunkat (x y z newchunk)
  (setf
   (gethash (chunkhashfunc x y z) chunkhash)
   newchunk)
  newchunk)

(defun chunkexistsat (x y z)
  (getchunkat chunkhash x y z))

(defun get-chunk-block (chunk i j k)
  (aref chunk (+  i (* 16 (+ (* 16 j) k)))))

(defun set-chunk-block (chunk i j k new)
  (setf
   (aref chunk (+  i (* 16 (+ (* 16 j) k))))
   new))

(defun getblock (i j k)
  (multiple-value-bind (x xd) (floor i 16)
    (Multiple-value-bind (y yd) (floor j 16)
      (multiple-value-bind (z zd) (floor k 16)
	(let ((chunk (getchunkat chunkhash x y z)))
	  (if chunk
	      (get-chunk-block chunk xd yd zd)
	      0))))))

(defun getlight (i j k)
  (multiple-value-bind (x xd) (floor i 16)
    (Multiple-value-bind (y yd) (floor j 16)
      (multiple-value-bind (z zd) (floor k 16)
	(let ((chunk (getchunkat lighthash x y z)))
	  (if chunk
	      (get-chunk-block chunk xd yd zd)
	      15))))))

(defun skygetlight (i j k)
  (multiple-value-bind (x xd) (floor i 16)
    (Multiple-value-bind (y yd) (floor j 16)
      (multiple-value-bind (z zd) (floor k 16)
	(let ((chunk nil))
	  (setf chunk (getchunkat skylighthash x y z))
	  (if chunk
	      (round (* (get-chunk-block chunk xd yd zd) 15/15))
	      15))))))

(defun empty-chunk-at (x y z)
  (let ((oldchunk (getchunkat chunkhash x y z)))
    (if oldchunk
	(clearchunk oldchunk)
	(setchunkat x y z (getachunk)))))

(defun destroy-chunk-at (x y z)
  (let ((oldchunk (getchunkat chunkhash x y z)))
    (if oldchunk
	(progn
	  (remhash (chunkhashfunc x y z) chunkhash)
	  (free-chunk oldchunk)))))

(defun setblock (i j k blockid)
  (multiple-value-bind (x xd) (floor i 16)
    (multiple-value-bind (y yd) (floor j 16)
      (multiple-value-bind (z zd) (floor k 16)
	(let* ((chunk (getchunkat chunkhash x y z)))
	  (if (not chunk)
	      (setf chunk (empty-chunk-at x y z)))
	  (let ((old (get-chunk-block chunk xd yd zd)))
	    (if (/= old blockid)
		(set-chunk-block chunk xd yd zd (the (unsigned-byte 8) blockid))
		nil)))))))

(defun block-dirtify (i j k)
  (pushnew (list (ash i -4) (ash j -4) (ash k -4)) dirtychunks :test 'equal))

(defun dirtify (x y z)
  (pushnew (list x y z) dirtychunks :test 'equal))

(defun blockcoordtochunk (x y z)
  (declare (type fixnum x y z))
  (chunkhashfunc (ash x -4) (ash y -4) (ash z -4)))

(defun setblock-with-update (i j k blockid)
  (if (setblock i j k blockid)
      (block-dirtify i j k)))

(defun someseq (x y)
  (let* ((thechunk (helpchunk x y)))
    (if thechunk
	(let ((light (getlightlizz thechunk))
	      (blocks (getblockslizz thechunk))
	      (skylight (getskylightlizz thechunk)))
	  (let ((counter 0))
	    (dolist (n (sandbox::flat3-chunk light))
	      (destroy-chunk-at x counter y)
	      (setf (gethash (chunkhashfunc x counter y) lighthash) n)
	      (incf counter)))
	  (let ((counter 0))
	    (dolist (n (sandbox::flat3-chunk skylight))
	      (destroy-chunk-at x counter y)
	      (setf (gethash (chunkhashfunc x counter y) skylighthash) n)
	      (incf counter)))
	  (let ((counter 0))
	    (dolist (n (sandbox::flat3-chunk blocks))
	      (destroy-chunk-at x counter y)
	      (setf (gethash (chunkhashfunc x counter y) chunkhash) n)
	      (dirtify x counter y)
	      (incf counter)))))))

(defun flat3-chunk (data)
  "takes flat chunk data and packs it into a chunk" 
  (let ((ourans nil))
    (dotimes (wow 8)
      (let ((new-chunk (getachunk)))
	(dotimes (j 16)
	  (dotimes (i 16)
	    (dotimes (k 16)
	      (set-chunk-block new-chunk i j k
			       (elt data (+ (* i 16 128) (+ j (* 16 wow)) (* k 128)))))))
	(push new-chunk ourans)))
    (nreverse ourans)))

(defparameter atest (byte-read #P "/home/imac/.minecraft/saves/fun world/region/r.-1.-1.mcr"))

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

;;"Data" 
;;"Entities" 
;;"LastUpdate" 
;;"xPos" 
;;"zPos" 
;;"TileEntities" 
;;"TerrainPopulated" 
;;"SkyLight" 
;;"HeightMap" 
;;"BlockLight" 
;;"Blocks"
