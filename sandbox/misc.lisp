(in-package :sandbox)

(defun spill-hash (hash)
  (loop for key being the hash-keys of hash
     using (hash-value value)
     do (format t "~S ~S~%" key value)))


(defun averager (amount)
  (let ((the-array (make-array amount :element-type 'fixnum))
	(index 0)
	(tot 0))
    (lambda (x)
      (let ((old (aref the-array index)))
	(setf tot (+ tot x (- old)))
	(setf (aref the-array index) x))
      (setf index (mod (1+ index) amount))
      (values (/ (coerce tot 'single-float) amount) the-array))))

(defun fun-setup ()
  (color-grasses)
  (test-world)
  (erase-bottom))

(defun erase-bottom ()
  (dobox ((x 0 128) (y 0 64) (z -128 0))
	 (plain-setblock x y z 0 0)))

(defun test-world ()
  (dobox ((x 0 8) (y -8 0))
	 (someseq x y)))

(defun spawn ()
  (goto 64 80 -64))

(defun color-grasses ()
  (modify-greens 64 192)
  (modify-greens 80 192)
  (modify-greens 0 240))

(defun ubyte-mult (a b)
  (truncate (* a b) 256))

(defun multiply-into (vecinto other)
  (macrolet ((aux (a b num)
	       `(let ((at (aref ,a ,num))
		      (bt (aref ,b ,num)))
		  (setf (aref ,a ,num) (ubyte-mult at bt)))))
    (aux vecinto other 0)
    (aux vecinto other 1)
    (aux vecinto other 2)
    (aux vecinto other 3)))

;;;grass is 0 240
;;;leaves is [64 80] 192
(defun modify-greens (xpos ypos &optional
				  (color (imagewise:getapixel
					  0 255
					  (lget *g/image* "misc/grasscolor.png"))))
  (let ((terrain (lget *g/image* "terrain.png")))
    (dobox ((x xpos (+ 16 xpos)) (y ypos (+ 16 ypos)))
	   (multiply-into (imagewise:getapixel y x terrain) color))))


(defun complex-modulus (c)
  (sqrt (realpart (* c (conjugate c)))))

(defun shit (x)
  (print x global-output))

(defun force-quit ()
  (SB-THREAD:terminate-thread (lget *g/thread* :SON-OF-MAIN)))

(defun pos? ()
  (print (list *xpos* *ypos* *zpos*)))

(defun vel? ()
  (print (list *xvel* *yvel* *zvel*)))

(defun draw-fistbox ()
  ;;;;draw the fist hitbox
  (progn (with-slots ((minx aabbcc::minx) (miny aabbcc::miny) (minz aabbcc::minz)
		      (maxx aabbcc::maxx) (maxy aabbcc::maxy) (maxz aabbcc::maxz))
	     fist-aabb
	   (draw-box (+ minx fistx -0) (+  miny fisty -0) (+  minz fistz -0)
		     (+ maxx fistx -0) (+  maxy fisty -0) (+  maxz fistz -0)))))

(defun neighbors (x y z)
  (let ((tot 0))
    (macrolet ((aux (i j k)
		 `(unless (zerop (world:getblock (+ x ,i) (+ y ,j) (+ z ,k)))
		   (incf tot))))
      (aux 1 0 0)
      (aux -1 0 0)
      (aux 0 1 0)
      (aux 0 -1 0)
      (aux 0 0 1)
      (aux 0 0 -1))
    tot))

(defun aux-func2 (x dx)
  (if (zerop dx)
      most-positive-double-float
      (if (plusp dx)
	  (/ (- (floor (1+ x)) x) dx)
	  (/ (- (ceiling (1- x)) x) dx))))

;;move to the next closest integer in the direction of the delta
(defun step-next (x y z dx dy dz)
  (values (aux-func2 x dx)
	  (aux-func2 y dy)
	  (aux-func2 z dz)))

(defun aux-step-next (x y z dx dy dz)
  (mvb (i j k) (step-next x y z dx dy dz)
       (mvb (value i? j? k?) (smallest i j k)
	    (values value
		    (+ x (* dx value))
		    (+ y (* dy value))
		    (+ z (* dz value))
		    i?
		    j?
		    k?))))

(defun dosteps (x y z dx dy dz)
  (let ((total 1)
	(pluspdx (plusp dx))
	(pluspdy (plusp dy))
	(pluspdz (plusp dz)))
    (declare (ignorable pluspdx pluspdy pluspdz))
    (tagbody
       rep
       (mvb (ratio newx newy newz i? j? k?) (aux-step-next x y z dx dy dz)
	    (declare (ignorable i? j? k?))
	    (when i?
	      (if pluspdx nil))
	    (when j?
	      (if pluspdy nil))
	    (when k?
	      (if pluspdz nil))
	    (setf x newx y newy z newz)
	    (world:setblock (floor x) (floor y) (floor z) 2)
	    (print (list x y z i? j? k?))
	    (decf total ratio)
	    (when (minusp total) (go end))
	    (go rep))
       end)))

(defun smallest (i j k)
  (if (< i j)
      (if (< i k) ;;i < j j is out
	  (values i t nil nil)	  ;;; i < k and i < j
	  (if (= i k)
	      (values i t nil t) ;;;tied for smallest
	      (values k nil nil t)	     ;;; k < i <j
	      ))
      (if (< j k) ;;i>=j
	  (if (= i j)
	      (values i t t nil)
	      (values j nil t nil)) ;;j<k and i<=j k is nout
	  (if (= i k)
	      (values i t t t)
	      (if (= k j)
		  (values k nil t t)
		  (values k nil nil t))) ;;i>=j>=k 
	  )))

(defun aux-func (x dx)
  (if (zerop dx)
      nil
      (if (plusp dx)
	  (floor (1+ x))
	  (ceiling (1- x)))))


(defun aabb-collect-blocks (px py pz dx dy dz aabb func)
  (declare (ignorable aabb))
  (with-slots ((minx aabbcc::minx) (miny aabbcc::miny) (minz aabbcc::minz)
	       (maxx aabbcc::maxx) (maxy aabbcc::maxy) (maxz aabbcc::maxz)) aabb
    (let ((total 1))
      (let ((pluspdx (plusp dx))
	    (pluspdy (plusp dy))
	    (pluspdz (plusp dz))
	    (zeropdx (zerop dx))
	    (zeropdy (zerop dy))
	    (zeropdz (zerop dz)))
	(declare (ignorable pluspdx pluspdy pluspdz zeropdx zeropdy zeropdz))
	(let ((xoffset (if zeropdx 0 (if pluspdx maxx minx)))
	      (yoffset (if zeropdy 0 (if pluspdy maxy miny)))
	      (zoffset (if zeropdz 0 (if pluspdz maxz minz))))
	  (let ((x (+ px xoffset))
		(y (+ py yoffset))
		(z (+ pz zoffset)))
	    (tagbody
	     rep
	       (let ((i-next (aux-func x dx))
		     (j-next (aux-func y dy))
		     (k-next (aux-func z dz)))
		 (mvb (ratio i? j? k?) (smallest (if i-next
						     (/ (- i-next x) dx)
						     most-positive-single-float)
						 (if j-next
						     (/ (- j-next y) dy)
						     most-positive-single-float)
						 (if k-next
						     (/ (- k-next z) dz)
						     most-positive-single-float))	 
		      (let ((newx (if i? i-next (+ x (* dx ratio))))
			    (newy (if j? j-next (+ y (* dy ratio))))
			    (newz (if k? k-next (+ z (* dz ratio)))))
			(let ((aabb-posx (- newx xoffset))
			      (aabb-posy (- newy yoffset))
			      (aabb-posz (- newz zoffset)))
			  (when i?
			    (dobox ((j (floor (+ aabb-posy miny))
				       (ceiling (+ aabb-posy maxy)))
				    (k (floor (+ aabb-posz minz))
				       (ceiling (+ aabb-posz maxz))))
				   (funcall func (if pluspdx newx (1- newx)) j k)))
			  (when j?
			    (dobox ((i (floor (+ aabb-posx minx))
				       (ceiling (+ aabb-posx maxx)))
				    (k (floor (+ aabb-posz minz))
				       (ceiling (+ aabb-posz maxz))))
				   (funcall func i (if pluspdy newy (1- newy)) k)))
			  (when k?
			    (dobox ((j (floor (+ aabb-posy miny))
				       (ceiling (+ aabb-posy maxy)))
				    (i (floor (+ aabb-posx minx))
				       (ceiling (+ aabb-posx maxx))))
				   (funcall func i j (if pluspdz newz (1- newz))))))
			(setf x newx y newy z newz))
		      (decf total ratio)
		      (when (minusp total) (go end))
		      (go rep)))
	     end)))))))
