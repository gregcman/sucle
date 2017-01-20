(in-package :sandbox)

(defparameter atest nil)

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
	       (setf (world:getlight x y z) b))
	     xscaled 0 yscaled)
	    (sandbox::flat3-chunk
	     skylight
	     (lambda (x y z b)
	       (setf (world:skygetlight x y z) b))
	     xscaled 0 yscaled)
	    (sandbox::flat3-chunk
	     meta
	     (lambda (x y z b)
	       (setf (world:getmeta x y z) b))
	     xscaled 0 yscaled)
	    (sandbox::flat2-chunk
	     leheight
	     (lambda (x y b)
	       (setf (world::getheight x y) b))
	     xscaled yscaled)
	    (sandbox::flat3-chunk
	     blocks
	     (lambda (x y z b)
	       (setf  (world:getblock x y z) b))
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

(defun setatest (x)
  (prog2 (setf atest
	       (case x
		 (0 (pathwise:byte-read #P "/home/terminal256/.minecraft/saves/New World-/region/r.0.-1.mcr"))
		 (1 (pathwise:byte-read #P "/home/imac/.minecraft/saves/New World/region/r.0.1.mcr"))
 		 (2 cl-mc-shit::testchunk)))
      x))

(eval-when (:load-toplevel)
  (setatest 0))

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
