(in-package :sandbox)

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
