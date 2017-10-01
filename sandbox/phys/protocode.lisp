
#+nil


#+nil
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

#+nil
((defun aux-func (x dx)
   (if (zerop dx)
       nil
       (if (plusp dx)
	   (floor (1+ x))
	   (ceiling (1- x)))))


 (defun aux-func2 (x dx)
   (if (zerop dx)
       nil
       (if (plusp dx)
	   (ceiling x)
	   (floor x))))

 (defun floor2 (x)
   (1- (ceiling x)))
 (defun ceiling2 (x)
   (1+ (floor x)))

 (defun collie-fusace (func
		       newx newy newz
		       i? j? k?
		       minx maxx miny maxy minz maxz)
   (when i?
     (dobox ((j miny maxy)
	     (k minz maxz))
	    (funcall func newx j k)))
   (when j?
     (dobox ((i minx maxx)
	     (k minz maxz))
	    (funcall func i newy k)))
   (when k?
     (dobox ((j miny maxy)
	     (i minx maxx))
	    (funcall func i j newz))))

 (defun aabb-collect-blocks (px py pz dx dy dz aabb func)
   (with-slots ((minx aabbcc::minx) (miny aabbcc::miny) (minz aabbcc::minz)
		(maxx aabbcc::maxx) (maxy aabbcc::maxy) (maxz aabbcc::maxz)) aabb
     (block cya
       (let ((pluspdx (plusp dx))
	     (pluspdy (plusp dy))
	     (pluspdz (plusp dz))
	     (zeropdx (zerop dx))
	     (zeropdy (zerop dy))
	     (zeropdz (zerop dz)))
	 (when (and zeropdx zeropdy zeropdz)
	   (return-from cya))
	 (let ((xoffset (if zeropdx 0.0 (if pluspdx maxx minx)))
	       (yoffset (if zeropdy 0.0 (if pluspdy maxy miny)))
	       (zoffset (if zeropdz 0.0 (if pluspdz maxz minz))))
	   (let ((x (+ px xoffset))
		 (y (+ py yoffset))
		 (z (+ pz zoffset)))
	     (let ((b (gen-blockstepper x y z dx dy dz)))
	       (loop
		  (multiple-value-bind
			(newx newy newz i? j? k?)
		      (blcokstepper b)
		    (unless newx		       
		      (return))
		    (let ((aabb-posx (- newx xoffset))
			  (aabb-posy (- newy yoffset))
			  (aabb-posz (- newz zoffset)))
		      (let ((minx (floor2 (+ aabb-posx minx)))
			    (maxx (ceiling2 (+ aabb-posx maxx)))
			    (miny (floor2 (+ aabb-posy miny)))
			    (maxy (ceiling2 (+ aabb-posy maxy)))
			    (minz (floor2 (+ aabb-posz minz)))
			    (maxz (ceiling2 (+ aabb-posz maxz))))
			(collie-fusace func
				       (if pluspdx newx (1- newx))
				       (if pluspdy newy (1- newy))
				       (if pluspdz newz (1- newz))
				       i? j? k?
				       minx maxx
				       miny maxy
				       minz maxz))))))))))))

 (defstruct blockstepper
   x
   y
   z
   dx
   dy
   dz
   i-next
   j-next
   k-next
   (total 1.0))

 (defun gen-blockstepper (x y z dx dy dz)
   (make-blockstepper :x x
		      :y y
		      :z z
		      :dx dx
		      :dy dy
		      :dz dz
		      :i-next (aux-func2 x dx)
		      :j-next (aux-func2 y dy)
		      :k-next (aux-func2 z dz)))

 (defun blcokstepper (b)
   (block nil
     (let ((i-next (blockstepper-i-next b))
	   (j-next (blockstepper-j-next b))
	   (k-next (blockstepper-k-next b))
	   (dx (blockstepper-dx b))
	   (dy (blockstepper-dy b))
	   (dz (blockstepper-dz b))
	   (x (blockstepper-x b))
	   (y (blockstepper-y b))
	   (z (blockstepper-z b)))
       (let ((i? nil)
	     (j? nil)
	     (k? nil)
	     (ratio nil))
		       ;;;;find the shortest distance to the next axis-aligned surface,
		       ;;;;setting the ? vars to true if they are the closest
	 (let ((fooi (if i-next
			 (/ (- i-next x) dx)
			 nil))
	       (fooj (if j-next
			 (/ (- j-next y) dy)
			 nil))
	       (fook (if k-next
			 (/ (- k-next z) dz)
			 nil)))
	   (progn
	     (when fooi
	       (setf ratio (if ratio					    
			       (min ratio fooi)
			       fooi)))
	     (when fooj
	       (setf ratio (if ratio					    
			       (min ratio fooj)
			       fooj)))
	     (when fook
	       (setf ratio (if ratio					    
			       (min ratio fook)
			       fook))))
	   (progn
	     (when fooi
	       (when (= ratio fooi)
		 (setf i? t)))
	     (when fooj
	       (when (= ratio fooj)
		 (setf j? t)))
	     (when fook
	       (when (= ratio fook)
		 (setf k? t)))))

	 (let ((total (- (blockstepper-total b) ratio)))
	   (when (minusp total) (return nil))
	   (setf (blockstepper-total b) total))
	 (when (zerop (/ most-positive-single-float ratio))
	   (return nil))
	 (let ((newx (if i? i-next (+ x (* dx ratio))))
	       (newy (if j? j-next (+ y (* dy ratio))))
	       (newz (if k? k-next (+ z (* dz ratio)))))
	   
	   (multiple-value-prog1
	       (values newx newy newz i? j? k?)

	     (setf (blockstepper-x b) newx
		   (blockstepper-y b) newy
		   (blockstepper-z b) newz)
	     (setf (blockstepper-i-next b) (aux-func newx dx)
		   (blockstepper-j-next b) (aux-func newy dy)
		   (blockstepper-k-next b) (aux-func newz dz))))))))
 )
