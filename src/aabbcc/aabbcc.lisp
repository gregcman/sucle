(defpackage #:aabbcc
  (:use #:cl #:utility #:rectangle)
  (:export
   #:aabb-minx
   #:aabb-miny
   #:aabb-minz
   #:aabb-maxx
   #:aabb-maxy
   #:aabb-maxz)
  (:export
   #:type-collapser
   #:aabb-collide
   #:aabb-contact
   #:make-aabb))
(in-package #:aabbcc)

(defun %collision-type (x y z)
  (let ((minimum 1)
	(values #b000))
    (flet ((minimize (n value)
	     (when n
	       (cond ((> minimum n)
		      (setf minimum n)
		      (setf values value))
		     ((= minimum n)
		      (setf values (logior values value)))))))
      (minimize x #b100)
      (minimize y #b010)
      (minimize z #b001))
    ;;set the values to magic numbers to be compared
    (values minimum
	    values)))

(defmacro type-translator (type dx dy dz)
  (case type
    (:xyz `(if (= ,dx ,dy)
	       (if (= ,dx ,dz)
		   #b111 ;;all equal sitting on corner
		   #b110);;dx dy equal dz wins
	       (if (= ,dx ,dz)
		   #b101;; dx dz equal dy wins
		   (if (= ,dy ,dz)
		       #b011;; dy dz equal dx wins
		       (if (> ,dx ,dy)
			   (if (> ,dy ,dz)
			       #b001;;dx is largest dz is smallest, so bye dz
			       #b010;;dy is smallest
			       )
			   (if (> ,dx ,dz)
			       #b001;;dz smallest
			       #b100;;dx beaten twice
			       ))))))
    (:xy `(if (= ,dx ,dy)
	      #b110
	      (if (> ,dx ,dy)
		  #b010
		  #b100)))
    (:xz `(if (= ,dx ,dz)
	      #b101
	      (if (> ,dx ,dz)
		  #b001
		  #b100)))
    (:yz `(if (= ,dy ,dz)
	      #b011
	      (if (> ,dy ,dz)
		  #b001
		  #b010)))
    (:x #b100)
    (:y #b010)
    (:z #b001)
    (nil #b000)
    (otherwise `(%type-translator type dx dy dz))))

(defun %type-translator (type dx dy dz)
  (etouq
   `(case type
      ,@(mapcar (lambda (x)
		  `(,x (type-translator ,x dx dy dz)))
		'(:xyz :xy :xz :yz :x :y :z nil))
      (otherwise #b000))))

;;rationale: there cannot be an xy collision when there is an x face collision.
;;similarly, there cannot be a xyz when there is xy or a subset
(defun type-collapser (dx dy dz xyz? xy? xz? yz? x? y? z?)
  (let ((ddx (abs dx))
	(ddy (abs dy))
	(ddz (abs dz)))
    (let ((acc 0))
      (macrolet
	  ((add (type)
	     `(setf acc
		    (logior
		     (type-translator ,type ddx ddy ddz)))))
	(if xyz? (add :xyz))
	(if xy? (add :xy))
	(if xz? (add :xz))
	(if yz? (add :yz))
	(if x? (add :x))
	(if y? (add :y))
	(if z? (add :z)))
      acc)))

(defmacro with-touch-collector ((collect-fun collapse-fun min-ratio) &body body)
  (with-gensyms (acc acc2 invalids potential-minimum type nope register dx dy dz)
    `(let ((,acc #b0000000)
	   (,invalids #b0000000)
	   (,min-ratio 1.0))
       (flet ((,collect-fun (,potential-minimum ,type)
		(if (> ,potential-minimum ,min-ratio)
		    -1
		    (flet ((,register (,type ,nope)
			     (if (< ,potential-minimum ,min-ratio) 
				 (progn
				   (setf ,min-ratio ,potential-minimum
					 ,acc ,type
					 ,invalids ,nope)
				   0)
				 (progn
				   (setf ,acc (logior ,acc ,type)
					 ,invalids (logior ,invalids ,nope))
				   1))))
		      (case ,type
			(#b111 (,register #b1000000 ;xyz
					 #b0000000))
			(#b110 (,register #b0100000 ;xy
					 #b1000000))
			(#b101 (,register #b0010000 ;xz
					 #b1000000))
			(#b011 (,register #b0001000 ;yz
					 #b1000000))
			(#b100 (,register #b0000100 ;x
					 #b1110000)) 
			(#b010 (,register #b0000010 ;y
					 #b1101000))
			(#b001 (,register #b0000001 ;z
					 #b1011000))))))
	      (,collapse-fun (,dx ,dy ,dz)
		(let ((,acc2 (logandc2 ,acc ,invalids)))
		  (type-collapser
		   ,dx ,dy ,dz
		   (logtest ,acc2 #b1000000)
		   (logtest ,acc2 #b0100000)
		   (logtest ,acc2 #b0010000)
		   (logtest ,acc2 #b0001000)
		   (logtest ,acc2 #b0000100)
		   (logtest ,acc2 #b0000010)
		   (logtest ,acc2 #b0000001)))))
	 ,@body))))

;;;spits out three values which indicate movement in the x y z directions.
(defun %%collide (ax0 ay0 az0 ax1 ay1 az1 dx dy dz bx0 by0 bz0 bx1 by1 bz1)
  #+nil
  (print (list (list ax0 ay0 az0 ax1 ay1 az1)
	       (list dx dy dz)
	       (list bx0 by0 bz0 bx1 by1 bz1)))
  (macrolet ((checkface ((op facea faceb) diff (d1 d2) (mx0 my0 mx1 my1 nx0 ny0 nx1 ny1))
	       `(if (,op ,facea ,faceb)
		    (let* ((delta (- ,faceb ,facea))
			   (ddx (/ (* delta ,d1) ,diff))
			   (ddy (/ (* delta ,d2) ,diff))
			   (state (r-intersect (+ ddx ,mx0) (+ ddy ,my0)
					       (+ ddx ,mx1) (+ ddy ,my1)
					       ,nx0 ,ny0
					       ,nx1 ,ny1)))
		      ;;(print (list ,diff delta))
		      (if state
			  (if (case state
				((t) t)
				(br (and (minusp ,d2) (plusp ,d1)))
				(ur (and (plusp ,d2) (plusp ,d1)))
				(ul (and (plusp ,d2) (minusp ,d1)))
				(bl (and (minusp ,d2) (minusp ,d1)))
				(b (minusp ,d2))
				(r (plusp ,d1))
				(u (plusp ,d2))
				(l (minusp ,d1)))
			      (/ delta ,diff)))))))
    ;;no point to test axis when velocity is zero
    ;;when the velocity is nonzero, check the direction
    ;;if the direction is positive but surface 2 is in the
    ;;negative direction relative to surface 1 we discard and also the reverse
    (%collision-type
     (unless (zerop dx)
       (if (plusp dx)
	   (checkface (<= ax1 bx0) dx (dy dz) (ay0 az0 ay1 az1 by0 bz0 by1 bz1))
	   (checkface (>= ax0 bx1) dx (dy dz) (ay0 az0 ay1 az1 by0 bz0 by1 bz1))))
     (unless (zerop dy)
       (if (plusp dy)
	   (checkface (<= ay1 by0) dy (dx dz) (ax0 az0 ax1 az1 bx0 bz0 bx1 bz1))
	   (checkface (>= ay0 by1) dy (dx dz) (ax0 az0 ax1 az1 bx0 bz0 bx1 bz1))))
     (unless (zerop dz)
       (if (plusp dz)
	   (checkface (<= az1 bz0) dz (dx dy) (ax0 ay0 ax1 ay1 bx0 by0 bx1 by1))
	   (checkface (>= az0 bz1) dz (dx dy) (ax0 ay0 ax1 ay1 bx0 by0 bx1 by1)))))))

(defun %contact (ax0 ay0 az0 ax1 ay1 az1 bx0 by0 bz0 bx1 by1 bz1)
  (let ((acc #b000000))
    (macrolet
	((touch ((aface bface) (max0 may0 max1 may1 mbx0 mby0 mbx1 mby1))
	   `(and (= ,aface ,bface)
		 (eq t (r-intersect ,max0 ,may0 ,max1 ,may1 ,mbx0 ,mby0 ,mbx1 ,mby1))))
	 (add (form bitfield)
	   `(if ,form (setf acc (logior acc ,bitfield)))))
      (add (touch (ax1 bx0) (ay0 az0 ay1 az1 by0 bz0 by1 bz1)) #b100000) ;x+
      (add (touch (ax0 bx1) (ay0 az0 ay1 az1 by0 bz0 by1 bz1)) #b010000) ;x-
      (add (touch (ay1 by0) (ax0 az0 ax1 az1 bx0 bz0 bx1 bz1)) #b001000) ;y+
      (add (touch (ay0 by1) (ax0 az0 ax1 az1 bx0 bz0 bx1 bz1)) #b000100) ;y-
      (add (touch (az1 bz0) (ax0 ay0 ax1 ay1 bx0 by0 bx1 by1)) #b000010) ;z+
      (add (touch (az0 bz1) (ax0 ay0 ax1 ay1 bx0 by0 bx1 by1)) #b000001) ;z-
      )  
    acc))

(defstruct aabb
  (minx 0.0 :type single-float)
  (miny 0.0 :type single-float)
  (minz 0.0 :type single-float)
  (maxx 0.0 :type single-float)
  (maxy 0.0 :type single-float)
  (maxz 0.0 :type single-float))

(defun aabb-collide (aabb0 px0 py0 pz0 aabb1 px1 py1 pz1 vx vy vz)
  (%%collide (+ px0 (aabb-minx aabb0))
	     (+ py0 (aabb-miny aabb0))
	     (+ pz0 (aabb-minz aabb0))
	     (+ px0 (aabb-maxx aabb0))
	     (+ py0 (aabb-maxy aabb0))
	     (+ pz0 (aabb-maxz aabb0))	    
	     vx vy vz
	     (+ px1 (aabb-minx aabb1))
	     (+ py1 (aabb-miny aabb1))
	     (+ pz1 (aabb-minz aabb1))
	     (+ px1 (aabb-maxx aabb1))
	     (+ py1 (aabb-maxy aabb1))
	     (+ pz1 (aabb-maxz aabb1))))
;;face contact between two aabbs. edges and corners do not count
(defun aabb-contact (x0 y0 z0 aabb0 x1 y1 z1 aabb1)
  (%contact (+ x0 (aabb-minx aabb0)) (+ y0 (aabb-miny aabb0)) (+ z0 (aabb-minz aabb0))
	    (+ x0 (aabb-maxx aabb0)) (+ y0 (aabb-maxy aabb0)) (+ z0 (aabb-maxz aabb0))
	    (+ x1 (aabb-minx aabb1)) (+ y1 (aabb-miny aabb1)) (+ z1 (aabb-minz aabb1))
	    (+ x1 (aabb-maxx aabb1)) (+ y1 (aabb-maxy aabb1)) (+ z1 (aabb-maxz aabb1))))

(defun aabb-not-overlap (aabb1 x1 y1 z1 aabb2 x2 y2 z2)
  (or
   (<= (+ x1 (aabb-maxx aabb1))
       (+ x2 (aabb-minx aabb2)))
   (<= (+ x2 (aabb-maxx aabb2))
       (+ x1 (aabb-minx aabb1)))
   (<= (+ y1 (aabb-maxy aabb1))
       (+ y2 (aabb-miny aabb2)))
   (<= (+ y2 (aabb-maxy aabb2))
       (+ y1 (aabb-miny aabb1)))
   (<= (+ z1 (aabb-maxz aabb1))
       (+ z2 (aabb-minz aabb2)))
   (<= (+ z2 (aabb-maxz aabb2))
       (+ z1 (aabb-minz aabb1)))))

;;;;aabb and voxel iteration facilities below.

;;;Iterate the voxels which are possibly touching a face of the box.
;;;Do not iterate over touching corners or edges.
(defmacro get-blocks-around ((x y z aabb)
			  (x-var y-var z-var contact-var)
			  &body body)
  (with-gensyms (emit minx maxx miny maxy minz maxz i0 i1 j0 j1 k0 k1 i j k)
    (once-only (x y z aabb)
      `(flet ((,emit (,x-var ,y-var ,z-var ,contact-var)
		,@body))
	 (let ((,minx (+ (aabb-minx ,aabb) ,x))
	       (,maxx (+ (aabb-maxx ,aabb) ,x))
	       (,miny (+ (aabb-miny ,aabb) ,y))
	       (,maxy (+ (aabb-maxy ,aabb) ,y))
	       (,minz (+ (aabb-minz ,aabb) ,z))
	       (,maxz (+ (aabb-maxz ,aabb) ,z)))
	   (let ((,i0 (floor ,minx))
		 (,i1 (ceiling ,maxx))
		 (,j0 (floor ,miny))
		 (,j1 (ceiling ,maxy))
		 (,k0 (floor ,minz))
		 (,k1 (ceiling ,maxz)))
	     (dobox ((,j ,j0 ,j1)
		     (,k ,k0 ,k1))
		    (,emit (1- (ceiling ,minx)) ,j ,k #b100000)
		    (,emit (floor ,maxx) ,j ,k #b010000))
	     (dobox ((,i ,i0 ,i1)
		     (,k ,k0 ,k1))
		    (,emit ,i (1- (ceiling ,miny)) ,k #b001000)
		    (,emit ,i (floor ,maxy) ,k #b000100))
	     (dobox ((,i ,i0 ,i1)
		     (,j ,j0 ,j1))
		    (,emit ,i ,j (1- (ceiling ,minz)) #b000010)
		    (,emit ,i ,j (floor ,maxz) #b000001))))))))

;;;Iterate without redundancy over voxels
;;;which are possibly touching a face, edge, or vertex.
;;;Only do a face if the bitfield contact-state allows it.
;;;Per axis, do one face, a positive face if "flip" is nil
;;;and a negative face if "flip" is t.
(defmacro do-shell ((x0 x1 y0 y1 z0 z1 xflip yflip zflip contact-state)
		    (x-var y-var z-var contact-var)
		    &body body)
  (with-gensyms (emit
		 major-x major-y major-z
		 large-x large-y large-z
		 small-x small-y small-z
		 x y z)
    (once-only  (x0 x1 y0 y1 z0 z1 xflip yflip zflip contact-state)
      `(flet ((,emit (,x-var ,y-var ,z-var ,contact-var)
		,@body))
	 (let ((,major-x (if ,xflip ,x0 ,x1))
	       (,major-y (if ,yflip ,y0 ,y1))
	       (,major-z (if ,zflip ,z0 ,z1))
	       (,large-x (if ,xflip (1+ ,x1) ,x1))
	       (,small-x (if ,xflip (1+ ,x0) ,x0))
	       (,large-y (if ,yflip (1+ ,y1) ,y1))
	       (,small-y (if ,yflip (1+ ,y0) ,y0))
	       (,large-z (if ,zflip (1+ ,z1) ,z1))
	       (,small-z (if ,zflip (1+ ,z0) ,z0)))
	   ;;xyz
	   (when (logtest ,contact-state #b111)
	     (,emit ,major-x ,major-y ,major-z #b111))
	   ;;xy
	   (when (logtest ,contact-state #b110)
	     (dobox ((,z ,small-z ,large-z))
		    (,emit ,major-x ,major-y ,z #b110)))
	   ;;xz
	   (when (logtest ,contact-state #b101)
	     (dobox ((,y ,small-y ,large-y))
		    (,emit ,major-x ,y ,major-z #b101)))
	   ;;yz
	   (when (logtest ,contact-state #b011)
	     (dobox ((,x ,small-x ,large-x))
		    (,emit ,x ,major-y ,major-z #b011)))
	   ;;x
	   (when (logtest ,contact-state #b100)
	     (dobox ((,y ,small-y ,large-y)
		     (,z ,small-z ,large-z))
		    (,emit ,major-x ,y ,z #b100)))
	   ;;y
	   (when (logtest ,contact-state #b010)
	     (dobox ((,x ,small-x ,large-x)
		     (,z ,small-z ,large-z))
		    (,emit ,x ,major-y ,z #b010)))
	   ;;z
	   (when (logtest ,contact-state #b001)
	     (dobox ((,x ,small-x ,large-x)
		     (,y ,small-y ,large-y))
		    (,emit ,x ,y ,major-z #b001))))))))

;;;Trace from closest to farthest over voxels
;;;which are in the path of "aabb".
(defmacro aabb-collect-blocks ((px py pz dx dy dz aabb)
			       (x-var y-var z-var contact-var)
			       &body body)
  (with-gensyms
      (emit xnotp ynotp znotp minx miny minz maxx maxy maxz
	    xflip yflip zflip xoffset yoffset zoffset x y z total
	    i-next j-next k-next ratio min? fooi fooj fook i? j? k?
	    x0 y0 z0 aabb-posx aabb-posy aabb-posz
	    bmini bmaxi bminj bmaxj bmink bmaxk i j k contact
	    test-shell flags)
    (once-only (px py pz dx dy dz aabb)
      `(let ((,xnotp (zerop ,dx))
	     (,ynotp (zerop ,dy))
	     (,znotp (zerop ,dz)))
	 (unless (and ,xnotp ,ynotp ,znotp)	   
	   (let* ((,minx (aabb-minx ,aabb))
		  (,miny (aabb-miny ,aabb))
		  (,minz (aabb-minz ,aabb))
		  (,maxx (aabb-maxx ,aabb))
		  (,maxy (aabb-maxy ,aabb))
		  (,maxz (aabb-maxz ,aabb))
		  (,xflip (minusp ,dx))
		  (,yflip (minusp ,dy))
		  (,zflip (minusp ,dz))
		  (,xoffset (if ,xnotp 0.0 (if ,xflip ,minx ,maxx)))
		  (,yoffset (if ,ynotp 0.0 (if ,yflip ,miny ,maxy)))
		  (,zoffset (if ,znotp 0.0 (if ,zflip ,minz ,maxz)))
		  (,x (+ ,px ,xoffset))
		  (,y (+ ,py ,yoffset))
		  (,z (+ ,pz ,zoffset))
		  (,total 1))
	     (when ,xflip
	       (setf ,dx (- ,dx)
		     ,x (- 0 ,x)))
	     (when ,yflip
	       (setf ,dy (- ,dy)
		     ,y (- 0 ,y)))
	     (when ,zflip
	       (setf ,dz (- ,dz)
		     ,z (- 0 ,z)))
	     (labels
		 ((,emit (,x-var ,y-var ,z-var ,contact-var)
		    ,@body)
		  (,test-shell (,flags)
		      (let ((,x0 (if ,xflip (- ,x) ,x))
			    (,y0 (if ,yflip (- ,y) ,y))
			    (,z0 (if ,zflip (- ,z) ,z)))
			(let ((,aabb-posx (- ,x0 ,xoffset))
			      (,aabb-posy (- ,y0 ,yoffset))
			      (,aabb-posz (- ,z0 ,zoffset)))
			  (let ((,bmini (ceiling (+ ,aabb-posx ,minx)))
				(,bmaxi (floor (+ ,aabb-posx ,maxx)))
				(,bminj (ceiling (+ ,aabb-posy ,miny)))
				(,bmaxj (floor (+ ,aabb-posy ,maxy)))
				(,bmink (ceiling (+ ,aabb-posz ,minz)))
				(,bmaxk (floor (+ ,aabb-posz ,maxz))))
			    (do-shell ((1- ,bmini) ,bmaxi
				       (1- ,bminj) ,bmaxj
				       (1- ,bmink) ,bmaxk
				       ,xflip
				       ,yflip
				       ,zflip
				       ,flags)
				(,i ,j ,k ,contact)
			      (,emit ,i ,j ,k ,contact)))))))
	       (,test-shell #b111)
	       (let ((,i-next (ceiling ,x))
		     (,j-next (ceiling ,y))
		     (,k-next (ceiling ,z)))
		 (loop
		    ;;find the shortest distance to the next axis-aligned surface
		    (let ((,ratio most-positive-single-float)
			  (,min? #b000))
		      (unless ,xnotp
			(let ((,fooi (/ (- ,i-next ,x) ,dx)))
			  (if (> ,ratio ,fooi)
			      (setf ,ratio ,fooi
				    ,min? #b100)
			      (when (= ,ratio ,fooi)
				(setf ,min? (logior ,min? #b100))))))
		      (unless ,ynotp
			(let ((,fooj (/ (- ,j-next ,y) ,dy)))
			  (if (> ,ratio ,fooj)
			      (setf ,ratio ,fooj
				    ,min? #b010)
			      (when (= ,ratio ,fooj)
				(setf ,min? (logior ,min? #b010))))))
		      (unless ,znotp
			(let ((,fook (/ (- ,k-next ,z) ,dz)))
			  (if (> ,ratio ,fook)
			      (setf ,ratio ,fook
				    ,min? #b001)
			      (when (= ,ratio ,fook)
				(setf ,min? (logior ,min? #b001))))))

		      (decf ,total ,ratio)
		      (unless (plusp ,total)
			(return))
		      (let ((,i? (logtest ,min? #b100))
			    (,j? (logtest ,min? #b010))
			    (,k? (logtest ,min? #b001)))
			;;ratchet up the coordinates
			(unless ,xnotp
			  (setf ,x (if ,i? ,i-next (+ ,x (* ,dx ,ratio))))
			  (setf ,i-next (1+ (floor ,x))))
			(unless ,ynotp
			  (setf ,y (if ,j? ,j-next (+ ,y (* ,dy ,ratio))))
			  (setf ,j-next (1+ (floor ,y))))
			(unless ,znotp
			  (setf ,z (if ,k? ,k-next (+ ,z (* ,dz ,ratio))))
			  (setf ,k-next (1+ (floor ,z))))
			;;find the surface cubes
			(,test-shell ,min?))))))))))))
