(defpackage #:aabbcc
  (:use #:cl)
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


(defun %%collide (ax0 ay0 az0 ax1 ay1 az1 dx dy dz bx0 by0 bz0 bx1 by1 bz1)
  (multiple-value-bind (x y z)
      (%collide ax0 ay0 az0 ax1 ay1 az1 dx dy dz bx0 by0 bz0 bx1 by1 bz1)
    (%collision-type x y z)))

(defun %collision-type (x y z)
  "determines whether the collision is a face, corner,
edge, or no case"
  (let ((minimum 1)
	(values #b000))
     (flet ((minimize (n value)
	     (cond ((> minimum n)
		    (setf minimum n)
		    (setf values value))
		   ((= minimum n)
		    (setf values (logior values value))))))
      (when x
	(minimize x #b100))
      (when y
	(minimize y #b010))
      (when z
	(minimize z #b001)))
    ;;set the values to magic numnbers to be compared
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
  (utility:etouq
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
    (macrolet
	((add (type)
	   `(type-translator ,type ddx ddy ddz)))
      (logior
       (if xyz? (add :xyz) 0)
       (if xy? (add :xy) 0)
       (if xz? (add :xz) 0)
       (if yz? (add :yz) 0)
       (if x? (add :x) 0)
       (if y? (add :y) 0)
       (if z? (add :z) 0)))))


;;pattern of checking each face
(defmacro checkface ((op facea faceb) diff (d1 d2) (mx0 my0 mx1 my1 nx0 ny0 nx1 ny1))
  `(if (,op ,facea ,faceb)
       (let* ((delta (- ,faceb ,facea))
	      (ddx (/ (* delta ,d1) ,diff))
	      (ddy (/ (* delta ,d2) ,diff))
	      (state (r-intersect (+ ddx ,mx0) (+ ddy ,my0)
				  (+ ddx ,mx1) (+ ddy ,my1)
				  ,nx0 ,ny0
				  ,nx1 ,ny1)))
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
		 (/ delta ,diff))))))

;;;if the velocity is zero, there is no point to test it
;;;if it is nonzero, we check the direction
;;;if the direction is positive but surface 2 is in the
;;;negative direction relative to surface 1 we discard
;;;spits out three values which indicate movement in the x y z directions.
(defun %collide (ax0 ay0 az0 ax1 ay1 az1 dx dy dz bx0 by0 bz0 bx1 by1 bz1)
  (values
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
	 (checkface (>= az0 bz1) dz (dx dy) (ax0 ay0 ax1 ay1 bx0 by0 bx1 by1))))))

;;determine how rectangles intersect .0 
;;means the edges touch. positive number means there is space between
;;negative means it is past. the symbols u r l and b represent the top,
;;right, left, and bottom of the first rectangle. nil means none at all
;;t means an area intersection
(defun r-intersect (ax0 ay0 ax1 ay1 bx0 by0 bx1 by1)
  (let ((dbottom (- ay0 by1))
	(dright (- bx0 ax1))
	(dup (- by0 ay1))
	(dleft (- ax0 bx1)))
    (let ((db (zerop dbottom))
	  (dr (zerop dright))
	  (du (zerop dup))
	  (dl (zerop dleft)))
      (if (or db dr du dl)
	  (cond ((and du dr) 'ur)
		((and du dl) 'ul)
		((and db dl) 'bl)
		((and db dr) 'br)
		(db 'b)
		(dr 'r)
		(du 'u)
		(dl 'l))
	  (not (or (plusp dbottom)
		   (plusp dright)
		   (plusp dup)
		   (plusp dleft)))))))

;;face contact between two aabbs. edges and corners do not count
(defun aabb-contact (x0 y0 z0 aabb0 x1 y1 z1 aabb1)
  (%contact (+ x0 (aabb-minx aabb0)) (+ y0 (aabb-miny aabb0)) (+ z0 (aabb-minz aabb0))
	    (+ x0 (aabb-maxx aabb0)) (+ y0 (aabb-maxy aabb0)) (+ z0 (aabb-maxz aabb0))
	    (+ x1 (aabb-minx aabb1)) (+ y1 (aabb-miny aabb1)) (+ z1 (aabb-minz aabb1))
	    (+ x1 (aabb-maxx aabb1)) (+ y1 (aabb-maxy aabb1)) (+ z1 (aabb-maxz aabb1))))

(defun %contact (ax0 ay0 az0 ax1 ay1 az1 bx0 by0 bz0 bx1 by1 bz1)
  (macrolet
      ((touch ((aface bface) (max0 may0 max1 may1 mbx0 mby0 mbx1 mby1))
	      `(and (= ,aface ,bface)
		    (eq t (r-intersect ,max0 ,may0 ,max1 ,may1 ,mbx0 ,mby0 ,mbx1 ,mby1)))))
    (logior
     (if (touch (ax1 bx0) (ay0 az0 ay1 az1 by0 bz0 by1 bz1)) #b100000 0) ;;plusx
     (if (touch (ax0 bx1) (ay0 az0 ay1 az1 by0 bz0 by1 bz1)) #b010000 0)   ;;-x
     (if (touch (ay1 by0) (ax0 az0 ax1 az1 bx0 bz0 bx1 bz1)) #b001000 0) ;;plusy
     (if (touch (ay0 by1) (ax0 az0 ax1 az1 bx0 bz0 bx1 bz1)) #b000100 0)   ;;-y
     (if (touch (az1 bz0) (ax0 ay0 ax1 ay1 bx0 by0 bx1 by1)) #b000010 0)	;;+z
     (if (touch (az0 bz1) (ax0 ay0 ax1 ay1 bx0 by0 bx1 by1)) #b000001 0))))	;; -Z



(in-package :aabbcc)
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

;;less used items below

#+nil
(defun %%%collide (ax0 ay0 az0 ax1 ay1 az1 dx dy dz bx0 by0 bz0 bx1 by1 bz1)
  (%%collide ax0 ay0 az0
	     (+ ax1 ax0) (+ ay1 ay0) (+ az1 az0)
	     dx dy dz
	     bx0 by0 bz0
	     (+ bx1 bx0) (+ by1 by0) (+ bz1 bz0)))

#+nil
(defun testit (&rest args)
  (multiple-value-bind (a b c d)
      (apply #'%%collide args)
    (print (list a b c d))))

#+nil
(defun testit2 (&rest args)
  (multiple-value-bind (a b c d)
      (apply #'%%%collide args)
    (print (list a b c d))))

#+nil
(defun test-cases ()
  (print "unit cubes along the z axis")
  (testit2 0 0 0 1 1 1
	  0 0 0
	  1 0 0 1 1 1)
  (print "corner touching cubes")
  (testit2 0 0 0 1 1 1
	  0 0 0
	  1 1 1 1 1 1)
  (print "corner business")
  (testit2 0 0 0 1 1 1
	  5 5 5
	  1.5 1.5 1.5 1 1 1)
  (print "a test")
  (testit2 0 0 0 1 1 1
	  2 3 10
	  1 2 5 1 1 1))


(defpackage #:sandbox-sub
  (:use :cl :utility :application :struct-to-clos))
(in-package #:sandbox-sub)


(defun floor5 (x)
  (1- (ceiling x)))
(defun get-blocks-around (aabb-posx aabb-posy aabb-posz aabb func)
  (let ((aminx (aabbcc:aabb-minx aabb))
	(aminy (aabbcc:aabb-miny aabb))
	(aminz (aabbcc:aabb-minz aabb))
	(amaxx (aabbcc:aabb-maxx aabb))
	(amaxy (aabbcc:aabb-maxy aabb))
	(amaxz (aabbcc:aabb-maxz aabb)))
    (let ((minx (+ aminx aabb-posx))
	  (maxx (+ amaxx aabb-posx))
	  (miny (+ aminy aabb-posy))
	  (maxy (+ amaxy aabb-posy))
	  (minz (+ aminz aabb-posz))
	  (maxz (+ amaxz aabb-posz)))
      (dobox ((j (floor miny)
		 (ceiling maxy))
	      (k (floor minz)
		 (ceiling maxz)))
	     (funcall func (floor5 minx) j k)
	     (funcall func (floor maxx) j k))
      (dobox ((i (floor minx)
		 (ceiling maxx))
	      (k (floor minz)
		 (ceiling maxz)))
	     (funcall func i (floor5 miny) k)
	     (funcall func i (floor maxy) k))
      (dobox ((j (floor miny)
		 (ceiling maxy))
	      (i (floor minx)
		 (ceiling maxx)))
	     (funcall func i j (floor5 minz))
	     (funcall func i j (floor maxz))))))

(progn
 (defun aux-func (x dx)
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
 (defun aabb-collect-blocks (px py pz dx dy dz aabb func)
   (let ((minx (aabbcc:aabb-minx aabb))
	 (miny (aabbcc:aabb-miny aabb))
	 (minz (aabbcc:aabb-minz aabb))
	 (maxx (aabbcc:aabb-maxx aabb))
	 (maxy (aabbcc:aabb-maxy aabb))
	 (maxz (aabbcc:aabb-maxz aabb)))
     (block cya
       (let ((total 1))
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
	       (flet ((collide-surface (newx newy newz i? j? k?)
			(let ((aabb-posx (- newx xoffset))
			      (aabb-posy (- newy yoffset))
			      (aabb-posz (- newz zoffset)))
					;	 (print "a")
			  (when i?
			    (dobox ((j (floor2 (+ aabb-posy miny))
				       (ceiling2 (+ aabb-posy maxy)))
				    (k (floor2 (+ aabb-posz minz))
				       (ceiling2 (+ aabb-posz maxz))))
				   (funcall func (if pluspdx newx (1- newx)) j k)))
					;	 (print "b")
			  (when j?
			    (dobox ((i (floor2 (+ aabb-posx minx))
				       (ceiling2 (+ aabb-posx maxx)))
				    (k (floor2 (+ aabb-posz minz))
				       (ceiling2 (+ aabb-posz maxz))))
				   (funcall func i (if pluspdy newy (1- newy)) k)))
					;(print "c")
			  (when k?
			    (dobox ((j (floor2 (+ aabb-posy miny))
				       (ceiling2 (+ aabb-posy maxy)))
				    (i (floor2 (+ aabb-posx minx))
				       (ceiling2 (+ aabb-posx maxx))))
				   (funcall func i j (if pluspdz newz (1- newz))))))))
		 (let ((i-next (aux-func2 x dx))
		       (j-next (aux-func2 y dy))
		       (k-next (aux-func2 z dz)))
		   (tagbody
		    rep
					;     		   (print (list i-next j-next k-next))		     
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
			  ;;		 (print (list fooi fooj fook))
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

			(decf total ratio)
			(when (minusp total) (return-from cya))
			(when (zerop (/ most-positive-single-float ratio))
			  (return-from cya))
			(let ((newx (if i? i-next (+ x (* dx ratio))))
			      (newy (if j? j-next (+ y (* dy ratio))))
			      (newz (if k? k-next (+ z (* dz ratio)))))
			  
			  (collide-surface newx newy newz i? j? k?)

			  #+nil
			  (progn
			    (when i? (if (plusp dx)
					 (incf newx)
					 (decf newx)))
			    (when j? (if (plusp dy)
					 (incf newy)
					 (decf newy)))
			    (when k? (if (plusp dz)
					 (incf newz)
					 (decf newz))))

			  (setf x newx y newy z newz))

			(setf i-next (aux-func x dx)
			      j-next (aux-func y dy)
			      k-next (aux-func z dz)))
		      (go rep))))))))))))

;;;;;;;;;;;;;
(defun collide-world2 (aabb-gen-fnc x y z dx dy dz)
  (multiple-value-bind (new-x new-y new-z xyzclamp)
      (step-motion aabb-gen-fnc
		   x y z dx dy dz
		   (logior (if (zerop dx) #b100 #b000)
			   (if (zerop dy) #b010 #b000)
			   (if (zerop dz) #b001 #b000)))
    (multiple-value-bind (new-dx new-dy new-dz) (clamp-vec
						 dx dy dz
						 xyzclamp)
      (values new-x new-y new-z new-dx new-dy new-dz))))
(defmacro with-clamp-and-ratio ((clamp-var ratio-var) (get-collision-data px py pz vx vy vz)
				&body body)
  `(multiple-value-bind (,clamp-var ,ratio-var)
       (collapse-touch ,vx ,vy ,vz
		       (funcall ,get-collision-data ,px ,py ,pz ,vx ,vy ,vz))
     ,@body))
(defun step-motion (get-collision-data px py pz vx vy vz &optional (xyzclamp 0))
  (with-clamp-and-ratio (clamp ratio) (get-collision-data px py pz vx vy vz)
    (multiple-value-bind (newvx newvy newvz) (clamp-vec vx vy vz clamp)
      (let ((npx (+ px (* ratio vx)))
	    (npy (+ py (* ratio vy)))
	    (npz (+ pz (* ratio vz)))
	    (newclamp (logior clamp xyzclamp)))
	(if (and (zerop newvx)
		 (zerop newvy)
		 (zerop newvz))
	    (values npx npy npz newclamp)
	    (let ((whats-left (- 1 ratio)))
	      (step-motion
	       get-collision-data
	       npx
	       npy
	       npz
	       (* newvx whats-left)
	       (* newvy whats-left)
	       (* newvz whats-left)
	       newclamp)))))))

(defun clamp-vec (vx vy vz xyzclamp)
  (values
   (if (logtest #b100 xyzclamp) 0 vx)
   (if (logtest #b010 xyzclamp) 0 vy)
   (if (logtest #b001 xyzclamp) 0 vz)))

(struct->class
 (defstruct touch-collector
   (acc #b0000000)
   (invalids #b0000000)
   (min-ratio 1.0)))
(defun reset-touch-collector (touch-collector)
  (setf (touch-collector-acc touch-collector) #b0000000)
  (setf (touch-collector-invalids touch-collector) #b0000000)
  (setf (touch-collector-min-ratio touch-collector) 1.0))
(defun collect-touch (minimum type touch-collector)
  (let ((tot-min (touch-collector-min-ratio touch-collector)))
    (if (> minimum tot-min)
	(values nil nil)
	(with-let-mapped-places ((acc (touch-collector-acc touch-collector))
				 (invalids (touch-collector-invalids touch-collector)))
	  (let ((is-minimum? (< minimum tot-min)))
	    (when is-minimum?
	      (setf (touch-collector-min-ratio touch-collector) minimum)
	      (setf acc #b0000000)
	      (setf invalids #b0000000))
	    (flet ((register (type nope)
		     (setf acc (logior acc type))
		     (setf invalids (logior invalids nope))))
	      (case type
		(#b111 (register #b1000000
				 #b0000000))
		(#b110 (register #b0100000
				 #b1000000))
		(#b101 (register #b0010000
				 #b1000000))
		(#b011 (register #b0001000
				 #b1000000))
		(#b100 (register #b0000100
				 #b1110000)) 
		(#b010 (register #b0000010
				 #b1101000))
		(#b001 (register #b0000001
				 #b1011000))))
	    (values is-minimum? t))))))
(defun collapse-touch (dx dy dz touch-collector)
  (let ((acc (touch-collector-acc touch-collector)))
    (setf acc (logandc2 acc (touch-collector-invalids touch-collector)))
    (values
     (aabbcc:type-collapser
      dx dy dz 
      (logtest acc #b1000000)
      (logtest acc #b0100000)
      (logtest acc #b0010000)
      (logtest acc #b0001000)
      (logtest acc #b0000100)
      (logtest acc #b0000010)
      (logtest acc #b0000001))
     (touch-collector-min-ratio touch-collector))))

;;;;;;;;;
(defun collide-fucks (some-hooks)
  (let (aabb
	(touch-collector (make-touch-collector))
	(blockvec (make-array 0 :adjustable t :fill-pointer 0)))
    (flet ((bladd-x-y-z (x y z aabb)
	     (vector-push-extend x blockvec)
	     (vector-push-extend y blockvec)
	     (vector-push-extend z blockvec)
	     (vector-push-extend aabb blockvec)))
      (let ((hooks some-hooks))
	(values
	 (lambda (px py pz vx vy vz)
	   (reset-touch-collector touch-collector)
	   (setf (fill-pointer blockvec) 0)
	   (dolist (fun hooks)
	     (funcall fun #'bladd-x-y-z px py pz vx vy vz aabb))
	   (dobox
	    ((index 0 (fill-pointer blockvec) :inc 4))
	    (let ((foox (aref blockvec (+ 0 index)))
		  (fooy (aref blockvec (+ 1 index)))
		  (fooz (aref blockvec (+ 2 index)))
		  (fooaabb (aref blockvec (+ 3 index))))
	      (multiple-value-bind (minimum type)
		  (aabbcc:aabb-collide
		   aabb
		   px py pz
		   fooaabb
		   foox fooy fooz
		   vx vy vz)
		(collect-touch minimum type touch-collector))))
	   touch-collector)
	 (lambda (newaabb) (setf aabb newaabb)))))))

(defun make-contact-suite ()
  (let ((px 0.0)
	(py 0.0)
	(pz 0.0)
	(aabb nil))
    (let ((funs nil))
      (let ((acc 0))
	(labels
	    ((run (npx npy npz naabb)
	       (setf px npx
		     py npy
		     pz npz
		     aabb naabb)
	       (setf acc 0)
	       (dolist (fun funs)
		 (funcall fun #'add npx npy npz naabb))
	       acc)
	     (add (mx my mz maabb)
	       (setf
		acc
		(logior acc
			(aabbcc:aabb-contact px py pz aabb mx my mz maabb))))
	     (set-fun (newfun)
	       (setf funs newfun)))
	  (list 'full #'run
		'add #'add
		'set-fun #'set-fun))))))

(defun configure-contact-handler
    (fun &optional (data (make-contact-suite)))
  (let ((full (getf data 'full))
	(set-fun (getf data 'set-fun))
	(add (getf data 'add)))
    (funcall
     set-fun
     fun
    ;(mapcar (lambda (func) (funcall func add)) fun)
     )
    full))

;;;;;
(defun unit-pitch-yaw (result pitch yaw)
  (let ((cos-pitch (cos pitch)))
    (setf (aref result 0) (* cos-pitch (sin yaw))
	  (aref result 1) (sin pitch)
	  (aref result 2) (* cos-pitch (cos yaw))))
  result)

(struct->class
 (defstruct necking
   (yaw 0.0)
   (pitch 0.0)))

(defun lookaround2 (neck newyaw newpitch)
  (setf (necking-yaw neck) newyaw
	(necking-pitch neck) newpitch))
(defun necktovec (neck result-vec)
  (unit-pitch-yaw result-vec
		  (necking-pitch neck)
		  (necking-yaw neck)))

;;;;;;;

(struct->class
 (defstruct farticle
   (position (nsb-cga:vec 0.0 0.0 0.0))
   (position-old (nsb-cga:vec 0.0 0.0 0.0))
   (velocity (vector 0.0 0.0 0.0))))

(defun step-farticle (p)
  (let ((old (farticle-position-old p))
	(curr (farticle-position p)))
    (nsb-cga:%copy-vec old curr)))

(define-modify-macro *= (&rest args)
  *)

(defun physics (yaw dir farticle
		noclip gravity fly
		is-jumping is-sneaking
		contact-handler
		world-collision-fun
		configure-aabb-fun
		aabb)
  (let ((tickscale (/ 1.0 3.0)))
    (step-farticle farticle)
    (let ((vel (farticle-velocity farticle))
	  (pos (farticle-position farticle)))
      (let ((contact-state (if noclip
			       #b000000
			       (with-vec (px py pz) (pos)
				 (funcall contact-handler px py pz aabb)))))
	(let ((onground (logtest contact-state #b000100)))
	  (contact-handle contact-state vel)
	  (let ((speed (* 0.4 (expt tickscale 2))))
	    (with-vec (xvel yvel zvel) (vel symbol-macrolet)
	      (if fly
		  (progn
		    (when is-jumping
		      (incf yvel speed))
		    (when is-sneaking
		      (decf yvel speed)))
		  
		  (if onground
		      (when is-jumping
			(incf yvel (* 0.49 (expt tickscale 1))))
		      (setf speed (* speed 0.2))))
	      (when dir
		(let ((dir (+ dir yaw)))
		  (incf xvel (* speed (sin dir)))
		  (incf zvel (* speed (cos dir)))))))
	  (let ((fun (if noclip
			 (lambda (&rest args)
			   (declare (ignore args))
			   (values 1 #b000))
			 (progn
			   (funcall configure-aabb-fun aabb)
			   world-collision-fun))))
	    (with-vec (vx vy vz) (vel symbol-macrolet)
	      (with-vec (px py pz) (pos symbol-macrolet)
		(setf (values px py pz vx vy vz)
		      (collide-world2
		       fun
		       px py pz vx vy vz)))))
	  (let ((air-friction 0.98)
		(walking-friction (* 0.6 0.9)))
	    (with-vec (xvel yvel zvel) (vel symbol-macrolet)
	      (if fly
		  (progn
		    (setf air-friction 0.9)
		    (*= xvel air-friction)
		    (*= zvel air-friction))
		  (progn
		    (setf air-friction 0.98)
		    (cond (onground
			   (*= xvel walking-friction)
			   (*= zvel walking-friction))
			  (t (*= xvel 0.9)
			     (*= zvel 0.9)
			     ))))
	      (when (and (not onground)
			 gravity)
		(decf yvel (* 0.08 (expt tickscale 2))))
	      (*= yvel air-friction))))))))

(defun contact-handle (acc vel)
  (multiple-value-bind (i+ i- j+ j- k+ k-)
      (values (logtest acc #b100000)
	      (logtest acc #b010000)
	      (logtest acc #b001000)
	      (logtest acc #b000100)
	      (logtest acc #b000010)
	      (logtest acc #b000001))
    (with-vec (xvel yvel zvel) (vel symbol-macrolet)
      (etouq 
       (cons
	'progn
	(mapcar
	 (lambda (args)
	   (apply
	    (lambda (axis plus minus)
	      (alexandria:with-gensyms (var)
		`(let ((,var ,axis))
		   (when (or (and (plusp ,var) ,plus)
			     (and (minusp ,var) ,minus))
		     (setf ,axis 0.0)))))
	    args))
	 '((xvel i+ i-)
	   (yvel j+ j-)
	   (zvel k+ k-))))))))

(defparameter *block-aabb*
  (aabbcc:make-aabb
   :minx 0.0
   :miny 0.0
   :minz 0.0
   :maxx 1.0
   :maxy 1.0
   :maxz 1.0))

(defparameter *player-aabb*
  (aabbcc:make-aabb
   :minx -0.3
   :miny -1.5
   :minz -0.3
   :maxx 0.3
   :maxy 0.12
   :maxz 0.3))

(defun ahook ()
  (let ((vec (make-array 0 :adjustable t :fill-pointer 0)))
    (flet ((add-x-y-z (x y z)
	     (vector-push-extend x vec)
	     (vector-push-extend y vec)
	     (vector-push-extend z vec)))
      (lambda (bladd px py pz vx vy vz aabb)
	(setf (fill-pointer vec) 0)
	(aabb-collect-blocks
	 px py pz vx vy vz aabb
	 #'add-x-y-z)
	(dobox
	 ((index 0 (fill-pointer vec) :inc 3))
	 (let ((x (aref vec (+ 0 index)))
	       (y (aref vec (+ 1 index)))
	       (z (aref vec (+ 2 index))))
	   (when (aref mc-blocks:*iscollidable*
		       (world:getblock x y z))
	     (let ((foox x)
		   (fooy y)
		   (fooz z)
		   (fooaabb *block-aabb*))
	       (funcall bladd foox fooy fooz fooaabb)))))))))

(defun a-contact-fun ()
  (let ((vec (make-array 0 :adjustable t :fill-pointer 0)))
    (flet ((add-x-y-z (x y z)
	     (vector-push-extend x vec)
	     (vector-push-extend y vec)
	     (vector-push-extend z vec)))
      (lambda (collect px py pz aabb)
	(setf (fill-pointer vec) 0)
	(get-blocks-around px py pz aabb #'add-x-y-z)
	(dobox
	 ((index 0 (fill-pointer vec) :inc 3))
	 (let ((x (aref vec (+ 0 index)))
	       (y (aref vec (+ 1 index)))
	       (z (aref vec (+ 2 index))))	     
	   (when (aref mc-blocks:*iscollidable* (world:getblock x y z))
;;	     (plain-setblock x y z (+ 2 (random 4)) 0)
	     (funcall collect x y z *block-aabb*))))))))

(struct->class
 (defstruct entity
   particle
   neck
   hips
   aabb
   contact
   fly?
   gravity?
   clip?
   jump?
   sneak?
   collision-fun
   configure-collision-fun
   contact-fun))

(defun gentity ()
  (multiple-value-bind (collisionfun config-fun) (collide-fucks (list (ahook)))
    (make-entity :configure-collision-fun config-fun
		 :collision-fun collisionfun
		 :contact-fun (configure-contact-handler (list (a-contact-fun)))
		 :particle (make-farticle)
		 :neck (make-necking)
		 :aabb *player-aabb*
		 :hips nil
		 :contact #b000000
		 :fly? t
		 :gravity? nil
		 :clip? t
		 :jump? nil
		 :sneak? nil)))

(defun physentity (entity)
  (physics
   (necking-yaw (entity-neck entity))
   (entity-hips entity)
   (entity-particle entity)
   (not (entity-clip? entity))
   (entity-gravity? entity)
   (entity-fly? entity)
   (entity-jump? entity)
   (entity-sneak? entity)
   (entity-contact-fun entity)
   (entity-collision-fun entity)
   (entity-configure-collision-fun entity)
   (entity-aabb entity)))

(defparameter *fist-aabb*
     ;;;a very small cubic fist
  (aabbcc:make-aabb
   :minx -0.005
   :miny -0.005
   :minz -0.005
   :maxx 0.005
   :maxy 0.005
   :maxz 0.005))

;;;;150 ms delay for sprinting
;;;;player eye height is 1.5, subtract 1/8 for sneaking

;;gravity is (* -0.08 (expt tickscale 2)) 0 0
;;falling friction is 0.98
;;0.6 * 0.91 is walking friction

;;fov::
;;70 is normal
;;110 is quake pro



(defun farticle-to-camera (farticle camera fraction)
  (let ((curr (farticle-position farticle))
	(prev (farticle-position-old farticle)))
    (let ((vec (camera-matrix:camera-vec-position camera))
	  (cev (camera-matrix:camera-vec-noitisop camera)))
      (nsb-cga:%vec-lerp vec prev curr fraction)
      (nsb-cga:%vec* cev vec -1.0))))
(defun entity-to-camera (entity camera fraction)
  (necktovec (entity-neck entity)
		      (camera-matrix:camera-vec-forward camera))	  
  (farticle-to-camera (entity-particle entity)
		      camera
		      fraction))

(defun change-entity-neck (entity yaw pitch)
  (let ((neck (entity-neck entity)))
    (lookaround2 neck yaw pitch)))

(defparameter *fov*
  ((lambda (deg)
     (* deg (coerce (/ pi 180.0) 'single-float)))
   (nth 1 '(95 70))))

(defparameter *black* (make-instance 'application::render-area :height 2 :width 2
				     :x 0
				     :y 0))

;;;;;;;

(struct->class
 (defstruct fister
   (selected-block (vector 0 0 0))
   (normal-block (vector 0 0 0))
   (exists nil)
   (position (vector 0 0 0))
   fun))

(defun standard-fist (fist px py pz vx vy vz)
  (with-clamp-and-ratio (xyzclamp frac) ((fister-fun fist) px py pz vx vy vz)
    (if (= #b000 xyzclamp)
	(setf (fister-exists fist) nil)
	(progn
	  (macrolet ((setvec3d (vec x y z)
	     (let ((a (gensym)))
	       `(let ((,a ,vec))
		  (setf (aref ,a 0) ,x
			(aref ,a 1) ,y
			(aref ,a 2) ,z)))))
	    (let ((a (+ px (* frac vx)))
		  (b (+ py (* frac vy)))
		  (c (+ pz (* frac vz))))
	      (let ((dx (if (logtest xyzclamp #b100)
			    (if (plusp vx) 1 -1) 0))
		    (dy (if (logtest xyzclamp #b010)
			    (if (plusp vy) 1 -1) 0))
		    (dz (if (logtest xyzclamp #b001)
			    (if (plusp vz) 1 -1) 0)))
		(setvec3d (fister-selected-block fist)
			  (floor (+ dx a))
			  (floor (+ dy b))
			  (floor (+ dz c))))
	      (setvec3d (fister-position fist)
			a 
			b
			c)
	      (setvec3d (fister-normal-block fist)
			(floor a) 
			(floor b)
			(floor c))))
	  (setf (fister-exists fist) t)))))

(defun gen-fister (fist-aabb funs)
  (let ((fist (make-fister)))
    (multiple-value-bind (fun set-aabb)
	(collide-fucks funs)
	(setf (fister-fun fist)
	      fun)
	(funcall set-aabb fist-aabb))
    fist))
;;;;;;;;;;;;;;;;;;;;

;;;;


(defparameter *reloadables*
  '(gl-init
    ;blockshader-text
    ;blockshader
    ;terrain-png
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deflazy gl-init (gl-context)
  (declare (ignorable application::gl-context))
  (clrhash sandbox::*g/chunk-call-list*))

(defun per-frame (session)
  (declare (ignorable session))
  (getfnc 'gl-init)
  (render-stuff))

(defparameter *render-ticks* 0)
(defun render-stuff ()
  ((lambda (width height)
     (let ((camera *camera*))
       (setf (camera-matrix:camera-aspect-ratio camera)
	     (/ (coerce width 'single-float)
		(coerce height 'single-float))))
     (let ((render-area *render-area*))
       (setf (render-area-width render-area) (* width (/ 1.0 1.0)) 
	     (render-area-height render-area) (* height (/ 1.0 1.0))
	     (render-area-x render-area) 0
	     (render-area-y render-area) 0
	     )))
   window::*width* window::*height*)
  (glhelp::bind-default-framebuffer)
  (set-render-area *render-area*)
  (set-sky-color)
  (setf (camera-matrix:camera-fov *camera*) *fov*)
  (setf (camera-matrix:camera-frustum-far *camera*) (* 1024.0 256.0))
  (camera-matrix:update-matrices *camera*)
  ;;;render chunks
  (camera-shader *camera*)
  ;;;render crosshairs
;  #+nil
  (progn
    ((lambda (width height)
       (let ((render-area *black*))
	 (setf
	  (render-area-x render-area) (- (* width (/ 1.0 2.0)) 1.0) 
	  (render-area-y render-area) (- (* height (/ 1.0 2.0)) 1.0)
	  )))
     window::*width* window::*height*)
    (set-render-area *black*)
    (gl:clear-color 1.0 1.0 1.0 1.0)
    (gl:clear
     :color-buffer-bit
     ))
  (incf *render-ticks*))

(defun set-sky-color ()
  (let ((daytime sandbox::*daytime*))
    (let ((r (* daytime (aref *sky-color* 0)))
	  (g (* daytime (aref *sky-color* 1)))
	  (b (* daytime (aref *sky-color* 2))))
      (gl:clear-color r g b 1.0))))
(defparameter *sky-color*
  #+nil
  (vector 0.68 0.8 1.0)
  #+nil
  (map 'vector (lambda (x) (/ x 255.0)) (vector 97 138 255))
  ;#+nil
  (vector 0.0 0.0 0.0))
  
(defparameter *fog-ratio* 0.75)
#+nil
(defparameter *depth-buffer?* t)
;;when set to nil does not clear the depth buffer, but instead flip-flops
;;the depth function and matrix. has edge artefacts when using glclear for color

#+nil
(defparameter *mata*
  (nsb-cga:matrix*
   (nsb-cga:translate* 0.0 0.0 1.0)
   (nsb-cga:scale* 1.0 1.0 -1.0)))
#+nil
(cond (*depth-buffer?*
	   )
	  #+nil
	  (t
	   (gl:clear :color-buffer-bit)
	   (cond ((evenp *render-ticks*)
		  ;;	   (gl:clear-depth 0.0)
		  (gl:depth-func :greater)
		  (gl:depth-range 0.5 1.0)
		  (nsb-cga:%matrix* matrix
				      *mata* cam))
		 (t
		  ;;	   (gl:clear-depth 1.0)
		  (gl:depth-func :less)
		  (gl:depth-range 0.0 0.5)
		  (setf matrix cam)))))

(defparameter *temp-matrix* (nsb-cga:identity-matrix))
(defun camera-shader (camera)
  (declare (optimize (safety 3) (debug 3)))
  (glhelp::use-gl-program (getfnc 'blockshader))

  (let ((matrix
	 (camera-matrix:camera-matrix-projection-view-player camera)))
    (gl:clear-depth 1.0)
    (gl:clear
     :color-buffer-bit
     :depth-buffer-bit)
    (gl:depth-func :less)

    (glhelp:with-uniforms uniform (getfnc 'blockshader)
      (gl:uniform-matrix-4fv 
       (uniform :pmv)
       matrix
       nil)
      (flet ((fractionalize (x)
	       (alexandria:clamp x 0.0 1.0)))
	(let ((time sandbox::*daytime*))
	  (let ((x (fractionalize (* (aref *sky-color* 0) time)))
		(y (fractionalize (* (aref *sky-color* 1) time)))
		(z (fractionalize (* (aref *sky-color* 2) time))))
	    (%gl:uniform-3f (uniform :fogcolor)
			    x y z)
	    (gl:uniformfv (uniform :camera-pos)
			  (camera-matrix:camera-vec-position *camera*))
	    (%gl:uniform-1f (uniform :foglet)
			    (/ -1.0 (or 128 (camera-matrix:camera-frustum-far *camera*)) *fog-ratio*))
	    (%gl:uniform-1f (uniform :aratio)
			    (/ 1.0 *fog-ratio*)))))

      (progn
	(gl:uniformi (uniform :sampler) 0)
	(glhelp::set-active-texture 0)
	(gl:bind-texture :texture-2d
			 (glhelp::handle (getfnc 'terrain))
			 ))
      #+nil
      (gl:uniformf 
       (uniform :time)
       (float (/ (get-internal-real-time)
		 100.0)))))
  (gl:enable :depth-test)
  
  (gl:enable :cull-face)
  (gl:disable :blend)
  (sandbox::draw-world)
  (sandbox::designatemeshing))

;;seizures are so FUN!
#+nil
#(:clear :set :copy :copy-inverted
  :noop :invert :and :nand :or :nor
  :xor :equiv :and-reverse :and-inverted :or-reverse :or-inverted)
(progn
  (defun color-grasses (terrain color)
    (modify-greens 64 192 :color color :terrain terrain)
    (modify-greens 80 192 :color color :terrain terrain)
    (modify-greens 0 240 :color color :terrain terrain)
    terrain)
  (defun getapixel (h w image)
    (destructuring-bind (height width c) (array-dimensions image)
      (declare (ignore height))
      (make-array 4 :element-type (array-element-type image)
		  :displaced-to image
		  :displaced-index-offset (* c (+ w (* h width))))))

  #+nil
  (#(1742848/8775 2673664/8775 1079296/8775 255)
    (getapixel 0 0 grass-tint)
    (getapixel 255 255 grass-tint))
;;minecraft fog color sometimes (0.68 0.8 1.0)
  ;;  (progno #(113 174 70 255)  #(198 304 122 255))
;;;grass is 0 240
;;;leaves is [64 80] 192
  (defun modify-greens (xpos ypos
			&key
			  (color #(0 0 0 0))
			  (terrain (error "no image")))
    (dobox ((x xpos (+ 16 xpos)) (y ypos (+ 16 ypos)))
	   ((lambda (vecinto other)
	      (map-into vecinto (lambda (a b) (truncate (* a b) 256)) vecinto other))
	    (getapixel (- 255 y) x terrain) color))))

;;;;load a png image from a path

(defun load-png (filename)
  (opticl:read-png-file filename))

(defvar *ourdir* (filesystem-util:this-directory))

(deflazy terrain-png ()
  (load-png 
   (filesystem-util:rebase-path #P"terrain.png" *ourdir*)))

(deflazy modified-terrain-png (terrain-png grass-png)
  (color-grasses
   (alexandria::copy-array terrain-png)
   (let ((value (random 256)))
     (getapixel value (random (1+ value)) grass-png))))

(deflazy terrain (modified-terrain-png gl-context)
  (make-instance
   'glhelp::gl-texture
   :handle
   (prog1
       (glhelp:pic-texture
	modified-terrain-png
	:rgba)
     (glhelp:apply-tex-params
      (quote ((:texture-min-filter . :nearest)
	      (:texture-mag-filter . :nearest)
	      (:texture-wrap-s . :repeat)
	      (:texture-wrap-t . :repeat)))))))
(deflazy grass-png ()
  (load-png 
   (filesystem-util:rebase-path #P"grasscolor.png" *ourdir*)))
(deflazy blockshader (blockshader-text gl-context)
  (glhelp::create-gl-program blockshader-text))

;;;; run use-sandbox before running application::main

;;example::
#+nil
(progn
  (ql:quickload :sandbox-application)
  (sndbx::use-sandbox)
  (sandbox::mload "third/")
  (main))

(deflazy blockshader-text ()
  (glslgen::ashader
   :version 120
   :vs
   (glslgen2::make-shader-stage
    :out '((color-out "float")
	   (texcoord-out "vec2")
	   
	   (fogratio-out "float"))
    :in '((position "vec4")
	  (texcoord "vec2")
	  (color "float")
	  (projection-model-view "mat4")

	  (foglet "float")
	  (aratio "float")
	  (camera-pos "vec3"))
    :program
    '(defun "main" void ()
      (= "gl_Position" (* projection-model-view position))
      (= color-out color)
      (= texcoord-out texcoord)

      (= fogratio-out (min 1.0 (+ (* foglet (distance camera-pos (|.| position "xyz"))) aratio)))))
   :frag
   (glslgen2::make-shader-stage
    :in '((texcoord "vec2")
	  (color "float")
	  (sampler "sampler2D")

	  (fogratio "float")
	  (fogcolor "vec3")
;;						     (wombo ("float" 4) "{2.0, 10.4, 1.0, 10.0}")
	  )
    :program
    '(defun "main" void ()
      (/**/ vec4 pixdata)
      (= pixdata ("texture2D" sampler texcoord))
      #+nil
      (if (> (|.| pixdata "g") 0.5)
	  (progn
	    "discard"))
      ;;      (= (|.| pixdata "rgb") (|.| pixdata "ggr"))
      (/**/ vec3 temp)
      (= temp 
       (mix 
	fogcolor
	(* color
	   (|.| pixdata "rgb"))
	fogratio
	))
      ;;      	 (*= temp ([] wombo (int (* 4.0 (|.| temp "g")))))
      (= (|.| :gl-frag-color "rgb") temp)))
   :attributes
   '((position . 2) 
     (texcoord . 8)
     (color . 0))
   :varyings
   '((color-out . color)
     (texcoord-out . texcoord)
     (fogratio-out . fogratio))
   :uniforms
   '((:pmv (:vertex-shader projection-model-view))
     (:fogcolor (:fragment-shader fogcolor))
     (:foglet (:vertex-shader foglet))
     (:aratio (:vertex-shader aratio))
     (:camera-pos (:vertex-shader camera-pos))
     (:sampler (:fragment-shader sampler)))))
