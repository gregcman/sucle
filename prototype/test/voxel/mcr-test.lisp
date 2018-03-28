(in-package :cl-nbt)

(defparameter *region-file*
  (case
      1
    (0 #P "/home/terminal256/.minecraft/saves/New World-/region/r.0.-1.mcr")
    (1 #P "/home/imac/.minecraft/saves/New World/region/r.0.1.mcr")
;    (2 (cl-mc-shit::testchunk))
    (3 #P "/home/imac/info/mcp/jars/saves/New World/region/r.-1.-1.mcr")
    ))

#+nil
(progno

 (defun setatest (x)
   (prog2 (setf atest
		)
       x))
 ;;(setatest 3)

 (defun someseq (x y)
   (let* ((thechunk (helpchunk (+ 24 x) y)))
     (if thechunk
	 (let ((light (getlightlizz thechunk))
	       (blocks (getblockslizz thechunk))
	       (skylight (getskylightlizz thechunk))
	       (meta (getmetadatalizz thechunk))
					;      (leheight (getheightlizz thechunk))
	       )
	   (let ((xscaled (ash x 4))
		 (yscaled (ash y 4)))
	     (progn (sandbox::flat3-chunk
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
		    (progno (sandbox::flat2-chunk
			     leheight
			     (lambda (x y b)
			       (setf (world::getheight x y) b))
			     xscaled yscaled)))
	     (sandbox::flat3-chunk
	      blocks
	      (lambda (x y z b)
		(unless (zerop b)
		  (setf  (world:getblock x y z) b)))
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
 )

#+nil
(defun test-world ()
  (dobox ((x 0 8) (y -8 0))
	 (someseq x y)))

(defparameter *region*
  nil)
;;"/home/imac/.minecraft/saves/dogeworld/region/r.0.0.mcr"
(defun atest ()
  (let ((handle (open-handle *region-file*)))
    (setf *region* handle)
    (test6)
    #+nil
    (dotimes (x 64)
      (dotimes (z 64)
	(dotimes (y 64)
	  (setblock (cond ((evenp y) 1)
			  ((evenp x) 2)
			  ((evenp z) 3)
			  (t 4))
		    x y z))))
    #+nil
    (map nil
	 (lambda (x)
	   (when x
	     (recalculate-heights x)))
	 (slot-value (data *region*) 'chunks))
 ;;   (print (flushall handle))
    (close-handle handle)))
(defun test6 ()
  (funland::dobox
   ((x0 0 8)
    (z0 0 8))
   (let ((x1 (* x0 16))
	 (z1 (* z0 16)))
     (funland::dobox
      ((x x1 (+ 16 x1))
       (z z1 (+ 16 z1))
       (y 0 128))
      (let ((x2 (+ (* 0 16) x))
	    (z2 (+ (* 0 16) z)))
	(let ((bid (getblock x2 y z2))
	      (bid1 (getlight x2 y z2))
	      (bid2 (skygetlight x2 y z2)))
	  (if (and (= bid 0)
		   (= bid1 0)
		   (= bid2 15))
	      nil
	      (progn
;		(princ "b")
		(world:setblock x y z bid)
		(world:setlight x y z bid1)
		(world:skysetlight x y z bid2)))))))))
#+nil
(atest::map-all-chunks
 (lambda (x y z)
   (let ((bid (world::getblock x y z))
	 (bid1 (world::getlight x y z))
	 (bid2 (world::skygetlight x y z)))
     (setblock bid x y (+ 128 z))
     (setlight bid1 x y (+ 128 z))
     (setskylight bid2 x y (+ 128 z)))))
(defun test3 ()
  (dotimes (x 2)
    (dotimes (y 2)
      #+nil
      (or
       (load-chunk x y *region*))
      (set-chunk (new-mcr-chunk x y) x y *region*)
      )))

(defun test ()
  (map nil
       (lambda (x)
	 (when x
	   (let ((y (payload (get-tag x "Level" "Blocks"
				      ))))
	     (map-into y (lambda (x)
			   (if (= 1 x) 0 x))
		       y))))
       (slot-value (data *region*) 'chunks)))


(defun chunk-mcr (x z)
  (values (floor x 32)
	  (floor z 32)))
(defun pos-chunk (x z)
  (values (floor x 16)
	  (floor z 16)))

(defun getblock (x y z)
  (setf y (mod y 128))
  (multiple-value-bind (chunkx chunkz) (pos-chunk x z)
    (let ((chunks (slot-value (data *region*) 'chunks)))
      (let ((intraid (pos-intra-chunkid chunkx chunkz)))
	(let ((chunk (aref chunks intraid)))
	  (unless chunk
	    (setf chunk (or (load-chunk chunkx chunkz *region*) 
			    (return-from getblock 0)))
	    (set-chunk chunk chunkx chunkz *region*))
	  (let ((data
		 (payload (get-tag chunk "Level" "Blocks"))))
	    (aref data (array-lookup x y z))))))))

(defun recalculate-heights (chunk)
  (let ((height (payload (get-tag chunk "Level" "HeightMap")))
	(blocks (payload (get-tag chunk "Level" "Blocks"))))
    (dotimes (x 16)
      (dotimes (z 16)
	(setheigt (getheight x z blocks) x z height)))))

(defun getheight (x z array)
  (let ((num 0))
    (loop for y from 127 downto 0 do
	 (unless (zerop (aref array (array-lookup x y z)))
	   (setf num (min 127 (1+ y)))
	   (return)))
    num))

(defun setheigt (value x z data)
  (setf 
   (aref data (HeightMap-lookup x z))
   value))

(defun setblock (value x y z)
  (setf y (mod y 128))
  (multiple-value-bind (chunkx chunkz) ;;chunk coordinates
      (pos-chunk x z)
    (let ((chunks (slot-value (data *region*) 'chunks)))
      (let ((intraid (pos-intra-chunkid chunkx chunkz)))
	(let ((chunk (aref chunks intraid)))
	  (unless chunk
	    (setf chunk (new-mcr-chunk chunkx chunkz))
	    (set-chunk chunk chunkx chunkz *region*))
	  (let ((data
		 (payload (get-tag chunk "Level" "Blocks"))))
	    (setf (aref data (array-lookup x y z))
		  value)))))))
(defun setlight (value x y z)
  (setf y (mod y 128))
  (multiple-value-bind (chunkx chunkz) ;;chunk coordinates
      (pos-chunk x z)
    (let ((chunks (slot-value (data *region*) 'chunks)))
      (let ((intraid (pos-intra-chunkid chunkx chunkz)))
	(let ((chunk (aref chunks intraid)))
	  (unless chunk
	    (setf chunk (new-mcr-chunk chunkx chunkz))
	    (set-chunk chunk chunkx chunkz *region*))
	  (let ((data
		 (payload (get-tag chunk "Level" "BlockLight")))
		(place (array-lookup x y z)))
	    (let ((place2 (ash place -1)))
	      (let ((old (aref data place2)))
		(setf (aref data place2)
		      (if (oddp place)
			  (dpb value (byte 4 4) old)
			  (dpb value (byte 4 0) old)))))))))))
(defun getlight (x y z)
  (setf y (mod y 128))
  (multiple-value-bind (chunkx chunkz) ;;chunk coordinates
      (pos-chunk x z)
    (let ((chunks (slot-value (data *region*) 'chunks)))
      (let ((intraid (pos-intra-chunkid chunkx chunkz)))
	(let ((chunk (aref chunks intraid)))
	  (unless chunk
	    (setf chunk (or (load-chunk chunkx chunkz *region*) 
			    (return-from getlight 0)))
	    (set-chunk chunk chunkx chunkz *region*))	  
	  (let ((data
		 (payload (get-tag chunk "Level" "BlockLight")))
		(place (array-lookup x y z)))
	    (let ((place2 (ash place -1)))
	      (let ((ans (aref data place2)))
		(if (oddp place)
		    (ldb (byte 4 4) ans)
		    (ldb (byte 4 0) ans))))))))))
(defun skygetlight (x y z)
  (setf y (mod y 128))
  (multiple-value-bind (chunkx chunkz) ;;chunk coordinates
      (pos-chunk x z)
    (let ((chunks (slot-value (data *region*) 'chunks)))
      (let ((intraid (pos-intra-chunkid chunkx chunkz)))
	(let ((chunk (aref chunks intraid)))
	  (unless chunk
	    (setf chunk (or (load-chunk chunkx chunkz *region*) 
			    (return-from skygetlight 0)))
	    (set-chunk chunk chunkx chunkz *region*))
	  
	  (let ((data
		 (payload (get-tag chunk "Level" "SkyLight")))
		(place (array-lookup x y z)))
	    (let ((place2 (ash place -1)))
	      (let ((ans (aref data place2)))
		(if (oddp place)
		    (ldb (byte 4 4) ans)
		    (ldb (byte 4 0) ans))))))))))
(defun skygetlight (x y z)
  (setf y (mod y 128))
  (multiple-value-bind (chunkx chunkz) ;;chunk coordinates
      (pos-chunk x z)
    (let ((chunks (slot-value (data *region*) 'chunks)))
      (let ((intraid (pos-intra-chunkid chunkx chunkz)))
	(let ((chunk (aref chunks intraid)))
	  (unless chunk
	    (setf chunk (or (load-chunk chunkx chunkz *region*) 
			    (return-from skygetlight 15)))
	    (set-chunk chunk chunkx chunkz *region*))
	  (let ((data
		 (payload (get-tag chunk "Level" "SkyLight")))
		(place (array-lookup x y z)))
	    (let ((place2 (ash place -1)))
	      (let ((ans (aref data place2)))
		(if (oddp place)
		    (ldb (byte 4 4) ans)
		    (ldb (byte 4 0) ans))))))))))
