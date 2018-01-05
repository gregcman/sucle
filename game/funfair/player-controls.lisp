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
   #:step-motion
   #:clamp-vec
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

(defun step-motion (get-collision-data px py pz vx vy vz &optional xclamp yclamp zclamp)
  (multiple-value-bind (ratio xc yc zc)
      (funcall get-collision-data px py pz vx vy vz)
    (multiple-value-bind (newvx newvy newvz)
	(clamp-vec vx vy vz xc yc zc)
      (let ((npx (+ px (* ratio vx)))
	    (npy (+ py (* ratio vy)))
	    (npz (+ pz (* ratio vz))))
	(if (and (zerop newvx)
		 (zerop newvy)
		 (zerop newvz))
	    (values npx npy npz (or xc xclamp) (or yc yclamp) (or zc zclamp))
	    (let ((whats-left (- 1 ratio)))
	      (step-motion
	       get-collision-data
	       npx
	       npy
	       npz
	       (* newvx whats-left)
	       (* newvy whats-left)
	       (* newvz whats-left)
	       (or xclamp xc)
	       (or yclamp yc)
	       (or zclamp zc))))))))

(defun clamp-vec (vx vy vz xclamp yclamp zclamp)
  (values
   (if xclamp 0 vx)
   (if yclamp 0 vy)
   (if zclamp 0 vz)))

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
  (let ((minimum 1))
      (if (numberp x)
	  (setq minimum (min minimum x))
	  (setq x -69))
      (if (numberp y)
	  (setq minimum (min minimum y))
	  (setq y -42))
      (if (numberp z)
	  (setq minimum (min minimum z))
	  (setq z -64))
      ;;set the values to magic numnbers to be compared
      (let ((xt (= minimum x))
	    (yt (= minimum y))
	    (zt (= minimum z)))
	(values minimum
		(if xt
		    (if yt
			(if zt
			    :xyz
			    :xy)
			(if zt
			    :xz
			    :x))
		    (if yt
			(if zt
			    :yz
			    :y)
			(if zt
			    :z
			    nil)))))))

(defun type-translator (type ddx ddy ddz)
  (let ((dx (abs ddx))
	(dy (abs ddy))
	(dz (abs ddz)))
    (case type
      (:xyz (if (= dx dy)
	       (if (= dx dz)
		   (values t t t) ;;all equal sitting on corner
		   (values t t nil));;dx dy equal dz wins
	       (if (= dx dz)
		   (values t nil t);; dx dz equal dy wins
		   (if (= dy dz)
		       (values nil t t);; dy dz equal dx wins
		       (if (> dx dy)
			   (if (> dy dz)
			       (values nil nil t);;dx is largest dz is smallest, so bye dz
			       (values nil t nil);;dy is smallest
			       )
			   (if (> dx dz)
			       (values nil nil t);;dz smallest
			       (values t nil nil);;dx beaten twice
			       ))))))
      (:xy (if (= dx dy)
	      (values t t nil)
	      (if (> dx dy)
		  (values nil t nil)
		  (values t nil nil))))
      (:xz (if (= dx dz)
	      (values t nil t)
	      (if (> dx dz)
		  (values nil nil t)
		  (values t nil nil))))
      (:yz (if (= dy dz)
	      (values nil t t)
	      (if (> dy dz)
		  (values nil nil t)
		  (values nil t nil))))
      (:x (values t nil nil))
      (:y (values nil t nil))
      (:z (values nil nil t))
      (nil (values nil nil nil)))))


;;rationale: there cannot be an xy collision when there is an x face collision.
;;similarly, there cannot be a xyz when there is xy or a subset
(defun type-collapser (dx dy dz xyz? xy? xz? yz? x? y? z?)
  (if (or xy? xz? yz? x? y? z?)
      (setq xyz? nil))
  (if (or x? y?)
      (setq xy? nil))
  (if (or x? z?)
      (setq xz? nil))
  (if (or y? z?)
      (setq yz? nil))
  (let ((xclamp nil)
	(yclamp nil)
	(zclamp nil))
    (macrolet
	((add (type)
	   `(multiple-value-bind (xc yc zc)
		(type-translator ,type dx dy dz)
	      (if xc (setq xclamp t))
	      (if yc (setq yclamp t))
	      (if zc (setq zclamp t)))))
      (if xyz? (add :xyz))
      (if xy? (add :xy))
      (if xz? (add :xz))
      (if yz? (add :yz))
      (if x? (add :x))
      (if y? (add :y))
      (if z? (add :z)))
    (values xclamp yclamp zclamp)))


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


(defpackage #:sndbx
  (:use :cl :funland :funfair))
(in-package #:sndbx)
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
  (multiple-value-bind (new-x new-y new-z xclamp yclamp zclamp)
      (aabbcc:step-motion aabb-gen-fnc
			   x y z dx dy dz)
    (multiple-value-bind (new-dx new-dy new-dz) (aabbcc:clamp-vec
						 dx dy dz
						 xclamp yclamp zclamp)
      (values new-x new-y new-z new-dx new-dy new-dz))))

(defun untouched (ratio)
  (= ratio 69.0))

(defstruct touch-collector
  (acc #b0000000)
  (min-ratio 1.0))
(defun reset-touch-collector (taco)
  (setf (touch-collector-acc taco) #b0000000)
  (setf (touch-collector-min-ratio taco) 1.0))
(define-modify-macro logiorf (&rest args)
  logior)
(defun collect-touch (minimum type touch-collector)
  (let ((tot-min (touch-collector-min-ratio touch-collector)))
    (if (> minimum tot-min)
	(values nil nil)
	(with-let-mapped-places ((acc (touch-collector-acc touch-collector)))
	  (let ((is-minimum? (< minimum tot-min)))
	    (when is-minimum?
	      (setf (touch-collector-min-ratio touch-collector) minimum)
	      (setf acc #b0000000))
	    (case type
	      (:xyz (logiorf acc #b1000000))
	      (:xy  (logiorf acc #b0100000))
	      (:xz  (logiorf acc #b0010000))
	      (:yz  (logiorf acc #b0001000))
	      (:x   (logiorf acc #b0000100))
	      (:y   (logiorf acc #b0000010))
	      (:z   (logiorf acc #b0000001)))
	    (values is-minimum? t))))))

(defun collapse-touch (dx dy dz touch-collector)
  (let ((acc (touch-collector-acc touch-collector)))
    (aabbcc:type-collapser
     dx dy dz 
     (logtest acc #b1000000)
     (logtest acc #b0100000)
     (logtest acc #b0010000)
     (logtest acc #b0001000)
     (logtest acc #b0000100)
     (logtest acc #b0000010)
     (logtest acc #b0000001))))

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
		 (funcall fun npx npy npz naabb))
	       acc)
	     (add (mx my mz maabb)
	       (logiorf
		acc
		(aabbcc:aabb-contact px py pz aabb mx my mz maabb)))
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
     (mapcar (lambda (func) (funcall func add)) fun))
    full))


(defun collide-fucks (some-hooks)
  (let (aabb
	(taco (make-touch-collector))
	(blockvec (make-array 0 :adjustable t :fill-pointer 0)))
    (flet ((bladd-x-y-z (x y z aabb)
	     (vector-push-extend x blockvec)
	     (vector-push-extend y blockvec)
	     (vector-push-extend z blockvec)
	     (vector-push-extend aabb blockvec)))
      (let ((hooks (mapcar (lambda (func)
			    (funcall func #'bladd-x-y-z))
			   some-hooks)))
	(values
	 (lambda (px py pz vx vy vz)
	   (reset-touch-collector taco)
	   (setf (fill-pointer blockvec) 0)
	   (dolist (fun hooks)
	     (funcall fun px py pz vx vy vz aabb))
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
		(collect-touch minimum type taco))))
	   (multiple-value-bind (xclamp yclamp zclamp)
	       (collapse-touch vx vy vz taco)
	     (values
	      (touch-collector-min-ratio taco)
	      xclamp yclamp zclamp)))
	 (lambda (newaabb) (setf aabb newaabb)))))))

;;;;;
(defun unit-pitch-yaw (result pitch yaw)
  (let ((cos-pitch (cos pitch)))
    (setf (aref result 0) (* cos-pitch (sin yaw))
	  (aref result 1) (sin pitch)
	  (aref result 2) (* cos-pitch (cos yaw))))
  result)

(defstruct necking
  (yaw 0.0)
  (pitch 0.0))

(defun lookaround2 (neck newyaw newpitch)
  (setf (necking-yaw neck) newyaw
	(necking-pitch neck) newpitch))
(defun necktovec (neck result-vec)
  (unit-pitch-yaw result-vec
		  (necking-pitch neck)
		  (necking-yaw neck)))

;;;;;;;
(defstruct fister
  (selected-block (vector 0 0 0))
  (normal-block (vector 0 0 0))
  (exists nil)
  (position (vector 0 0 0))
  fun)

(defmacro setvec3d (vec x y z)
  (let ((a (gensym)))
    `(let ((,a ,vec))
       (setf (aref ,a 0) ,x
	     (aref ,a 1) ,y
	     (aref ,a 2) ,z))))

(defun standard-fist (fist px py pz vx vy vz)
  (multiple-value-bind (frac xclamp yclamp zclamp)
      (funcall (fister-fun fist) px py pz vx vy vz)
    (if (or xclamp yclamp zclamp)
	(progn
	  (let ((a (+ px (* frac vx)))
		(b (+ py (* frac vy)))
		(c (+ pz (* frac vz))))
	    (let ((dx (if xclamp (if (plusp vx) 1 -1) 0))
		  (dy (if yclamp (if (plusp vy) 1 -1) 0))
		  (dz (if zclamp (if (plusp vz) 1 -1) 0)))
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
		      (floor c)))
	  (setf (fister-exists fist) t))
	(setf (fister-exists fist) nil))))

(defun use-fist (fist left-p right-p left-fun right-fun)
  (let ((fist? (fister-exists fist))
	(selected-block (fister-selected-block fist))
	(normal-block (fister-normal-block fist)))
    (when fist?
      (when left-p
	(funcall left-fun
		 (aref selected-block 0)
		 (aref selected-block 1)
		 (aref selected-block 2)))
      (when right-p
	(funcall right-fun
		 (aref normal-block 0)
		 (aref normal-block 1)
		 (aref normal-block 2))))))

;;;;;;;;;;;;;;;;;;;;
(defstruct farticle
  (position (cg-matrix:vec 0.0 0.0 0.0))
  (position-old (cg-matrix:vec 0.0 0.0 0.0))
  (velocity (vector 0.0 0.0 0.0)))

(defun step-farticle (p)
  (let ((old (farticle-position-old p))
	(curr (farticle-position p)))
    (cg-matrix:%copy-vec old curr)))

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
			   (values 1 nil nil nil))
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

(defun ahook (bladd)
  (let ((vec (make-array 0 :adjustable t :fill-pointer 0)))
    (flet ((add-x-y-z (x y z)
	     (vector-push-extend x vec)
	     (vector-push-extend y vec)
	     (vector-push-extend z vec)))
      (lambda (px py pz vx vy vz aabb)
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

(defun a-contact-fun (collect)
  (let ((vec (make-array 0 :adjustable t :fill-pointer 0)))
    (flet ((add-x-y-z (x y z)
	     (vector-push-extend x vec)
	     (vector-push-extend y vec)
	     (vector-push-extend z vec)))
      (lambda (px py pz aabb)
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
  contact-fun)

(defun gentity ()
  (multiple-value-bind (collisionfun config-fun)
      (collide-fucks (list #'ahook))
    (make-entity :configure-collision-fun config-fun
		 :collision-fun collisionfun
		 :contact-fun (configure-contact-handler (list #'a-contact-fun))
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

(defun meta-controls (control-state entity)
  (symbol-macrolet ((pos (farticle-position (entity-particle entity)))
		    (is-jumping (entity-jump? entity))
		    (is-sneaking (entity-sneak? entity))
		    (fly (entity-fly? entity))
		    (gravity (entity-gravity? entity))
		    (noclip (entity-clip? entity)))
    (setf is-jumping (window::skey-p (window::keyval :space) control-state))
    (setf is-sneaking (window::skey-p (window::keyval :a) control-state))
    (when (window:mice-locked-p)
      (when (window::skey-j-p (window::keyval :v) control-state)
	(toggle noclip))
      (with-vec (x y z) (pos)
	(when (window::skey-j-p (window::keyval :p) control-state)
	  (sandbox::update-world-vao x y z)))
      (when (window::skey-j-p (window::keyval :g) control-state)
	(toggle fly)
	(toggle gravity)))))

(defparameter *fist-aabb*
     ;;;a very small cubic fist
  (aabbcc:make-aabb
   :minx -0.005
   :miny -0.005
   :minz -0.005
   :maxx 0.005
   :maxy 0.005
   :maxz 0.005))

(defun gen-fister (fist-aabb funs)
  (let ((fist (make-fister)))
    (multiple-value-bind (fun set-aabb)
	(collide-fucks funs)
	(setf (fister-fun fist)
	      fun)
	(funcall set-aabb fist-aabb))
    fist))
(defparameter *fist*
  (gen-fister *fist-aabb* (list #'ahook)))


(defparameter *reach* 128.0)
(defparameter *swinging* nil)
(defun use-fists (control-state look-vec pos)
  (let ((fist *fist*))
    (with-vec (px py pz) (pos)
      (with-vec (vx vy vz) (look-vec)	
	(when (window:mice-locked-p)
	  (when (window::skey-j-p (window::keyval :w) control-state)
	    (toggle *swinging*))
	  (when *swinging*
	    (big-swing-fist
	     px py pz
	     vx vy vz)))
	(let ((a (window::skey-j-p (window::mouseval :left) control-state))
	      (b (window::skey-j-p (window::mouseval :right) control-state)))
	  (when (or a b)
	    (standard-fist
	     fist
	     px py pz
	     (* *reach* vx) (* *reach* vy) (* *reach* vz))
	    (use-fist fist
		      a
		      b
		      *left-fist-fnc*
		      *right-fist-fnc*)))))))
(defparameter *right-fist-fnc*
  (lambda (x y z)
    (let ((blockval 1))
      (sandbox::plain-setblock
       x
       y
       z
       blockval
       (aref mc-blocks:*lightvalue* blockval)))))
(defparameter *left-fist-fnc*
  (lambda (x y z)
    (sandbox::setblock-with-update x y z 0 0)))

(defparameter *big-fist-fun*
  (or
   (lambda (x y z)
     (let ((a (world::getblock x y z)))
       (unless (or (= a 1)
		   (= a 0))
	 (sandbox::plain-setblock x y z 0 0))))
   ;;  #'atest::bonder2
   #+nil
   (lambda (x y z)
     (atest::dirts x y z)
     (atest::grassify x y z))
  ;; (atest::sheath 2 1)
   (lambda (x y z) ;(print (list x y z))
     )
   ))

(defun big-swing-fist (px py pz vx vy vz)
  (let ((u 128))
    (aabb-collect-blocks
     px py pz (* u vx) (* u vy) (* u vz)
     (load-time-value
      (aabbcc:make-aabb
       :minx -10.3
       :miny -10.5
       :minz -10.3
       :maxx 10.3
       :maxy 11.12
       :maxz 10.3))   
     *big-fist-fun*)))

;;;;150 ms delay for sprinting
;;;;player eye height is 1.5, subtract 1/8 for sneaking

;;gravity is (* -0.08 (expt tickscale 2)) 0 0
;;falling friction is 0.98
;;0.6 * 0.91 is walking friction

;;fov::
;;70 is normal
;;110 is quake pro



(defun wasd-mover (w? a? s? d?)
  (let ((x 0)
	(y 0))
    (when w? (decf x))
    (when a? (decf y))
    (when s? (incf x))
    (when d? (incf y))
    (if (and (zerop x)
	     (zerop y))
	nil
	(atan y x))))

(defun num-key-jp (control-state)
  (etouq
   (cons
    'cond
    (mapcar
     (lambda (n)
       `((window::skey-j-p
	  (window::keyval ,(intern (write-to-string n) :keyword))
	  control-state) ,n))
     '(0 1 2 3 4 5 6 7 8 9)))))

(defparameter *sandbox-on* t)
(setf sandbox::*some-saves*
      (cdr (assoc (machine-instance) 
		  '(("gm3-iMac" . #P"/home/imac/Documents/lispysaves/saves/sandbox-saves/")
		    ("nootboke" . #P"/home/terminal256/Documents/saves/"))
		  :test 'equal)))
(defparameter *ticker*
  (tickr:make-ticker
   (floor 1000000 60)
   most-positive-fixnum))

(defparameter *ents* (map-into (make-array 10) #'gentity))
(defparameter *ent* (aref *ents* 1))

(defun physss ()
  (let* ((player-farticle (entity-particle *ent*))
	 (pos (farticle-position player-farticle))
	 (control-state *control-state*))
    (meta-controls control-state
			    *ent*)
    (let ((num (num-key-jp *control-state*)))
      (when num
	(setf *ent* (aref *ents* num))))
    #+nil
    (when (window::skey-j-p (window::keyval :j) control-state)
       (atest::wowz))
    (setf (entity-hips *ent*)
	  (wasd-mover
	   (window::skey-p (window::keyval :e) control-state)
	   (window::skey-p (window::keyval :s) control-state)
	   (window::skey-p (window::keyval :d) control-state)
	   (window::skey-p (window::keyval :f) control-state)))
    (physentity *ent*)
    (let ((backwardsbug (load-time-value (cg-matrix:vec 0.0 0.0 0.0))))
      (cg-matrix:%vec* backwardsbug (camat:camera-vec-forward *camera*) -1.0)
      (use-fists control-state backwardsbug
			  pos))))

(defun farticle-to-camera (farticle camera fraction)
  (let ((curr (farticle-position farticle))
	(prev (farticle-position-old farticle)))
    (let ((vec (camat:camera-vec-position camera))
	  (cev (camat:camera-vec-noitisop camera)))
      (cg-matrix:%vec-lerp vec prev curr fraction)
      (cg-matrix:%vec* cev vec -1.0))))
(defun entity-to-camera (entity camera fraction)
  (necktovec (entity-neck entity)
		      (camat:camera-vec-forward camera))	  
  (farticle-to-camera (entity-particle entity)
		      camera
		      fraction))

(defun change-entity-neck (entity yaw pitch)
  (let ((neck (entity-neck entity)))
    (lookaround2 neck yaw pitch)))

(defparameter *fov*
  ((lambda (deg)
     (* deg (coerce (/ pi 180.0) 'single-float)))
   70))

(defparameter *black* (make-instance 'funfair::render-area :height 2 :width 2
				     :x 0
				     :y 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun moused (&optional (data (load-time-value (cons 0.0d0 0.0d0))))
  (multiple-value-bind (x y) (values window::*mouse-x* window::*mouse-y*)
    (multiple-value-prog1
	(values (- x (car data))
		(- y (cdr data)))
	(setf (car data) x
	      (cdr data) y))))
(defun update-moused (&optional (smoothing-factor 0.5))
  (multiple-value-bind (dx dy) (moused)
    (let ((x (+ *mouse-x* dx))
	  (y (+ *mouse-y* dy)))
      (let ((value *mouse-multiplier-aux*))
	(when (> y value)
	  (setf y value))
	(when (< y (- value))
	  (setf y (- value))))
      (setf *mouse-x* x)
      (setf *mouse-y* y)
      (setf *lerp-mouse-x* (alexandria:lerp smoothing-factor *lerp-mouse-x* x))
      (setf *lerp-mouse-y* (alexandria:lerp smoothing-factor *lerp-mouse-y* y)))))
(defparameter *mouse-x* 0.0d0)
(defparameter *mouse-y* 0.0d0)
(defparameter *lerp-mouse-x* 0.0d0)
(defparameter *lerp-mouse-y* 0.0d0)
(progn
  (declaim (ftype (function (single-float) single-float)
		  translator))
  (funland::with-unsafe-speed
    (defun translator (x)
      (let* ((a (* x 0.6))
	     (b (+ 0.2 a))
	     (c (* b b b))
	     (d (* 8.0 0.15 (/ (coerce pi 'single-float) 180.0)))
	     (e (* d c)))
	(declare (type single-float a b c d e))
	e))))

(defparameter *mouse-multiplier* (translator 0.5))
(defparameter *mouse-multiplier-aux* (/ (* 0.5 pi 0.9999) *mouse-multiplier*))


(defparameter *reloadables*
  '(blockshader-text
    blockshader
    terrain-png))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deflazy gl-init (funfair::gl-context)
  (declare (ignorable funfair::gl-context))
  (clrhash sandbox::*g/chunk-call-list*))

(defparameter *paused* nil)
(defun atick (session)
  (declare (ignorable session))
 ; (print 324234)
  (progn
    (funfair::reload-if-dirty 'gl-init)
 ;   (print 234234)
    (getfnc 'gl-init))
 ; (print 34243)
  (when (window::skey-j-p (window::keyval :r) *control-state*)
    (window:toggle-mouse-capture)
    (moused))
  
  (map nil #'funfair::reload-if-dirty *reloadables*)
  ((lambda (width height)
     (let ((camera *camera*))
       (setf (camat:camera-aspect-ratio camera)
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
  (gl:clear
   :color-buffer-bit
   :depth-buffer-bit
   )
  (setf (camat:camera-fov *camera*) *fov*)
  (setf (camat:camera-frustum-far *camera*) (* 1024.0 256.0))
  (when (window::skey-j-p (window::keyval :x))
    (toggle *paused*))
  (when *sandbox-on*
    (if *paused*
	(tick *ticker* (lambda ()))
	(multiple-value-bind (fraction times) (tick *ticker* #'physss)
	  (declare (ignorable times))
	  (when (window:mice-locked-p)
	    (update-moused 0.5)
	    (multiple-value-call
		#'change-entity-neck
	      *ent*
	      (multiple-value-bind (x y) (values *lerp-mouse-x*
						 *lerp-mouse-y*)
		(values (coerce (* x -1.0d0 *mouse-multiplier*)
				'single-float)
			(coerce (* y *mouse-multiplier*)
				'single-float)))
	      ))
	  (entity-to-camera *ent* *camera* fraction)))
    (camat:update-matrices *camera*)
    (camera-shader *camera*))
  
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
     )))

(defun set-sky-color ()
  (let ((daytime sandbox::*daytime*))
    (let ((r (* daytime (aref *sky-color* 0)))
	  (g (* daytime (aref *sky-color* 1)))
	  (b (* daytime (aref *sky-color* 2))))
      (gl:clear-color r g b 1.0))))
(defparameter *sky-color* #+nil (vector 0.68 0.8 1.0)
	      (vector 1.0 1.0 1.0))
(defparameter *fog-ratio* 0.75)

(defun camera-shader (camera)
  (declare (optimize (safety 3) (debug 3)))
  (glhelp::use-gl-program (getfnc 'blockshader))
  
  (glhelp:with-uniforms uniform (getfnc 'blockshader)
    (gl:uniform-matrix-4fv 
     (uniform :pmv)
     (camat:camera-matrix-projection-view-player camera)
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
			(camat:camera-vec-position *camera*))
	  (%gl:uniform-1f (uniform :foglet)
			  (/ -1.0 (or 128 (camat:camera-frustum-far *camera*)) *fog-ratio*))
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
	       100.0))))
  (gl:enable :depth-test)  
  (gl:depth-func :less)
  (gl:enable :cull-face)
  (gl:cull-face :back)
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

(deflazy terrain-png (grass-png)
  (color-grasses
   (load-png 
    (filesystem-util:rebase-path #P"terrain.png" *ourdir*))
   (getapixel 255 255 grass-png)))
(deflazy terrain (terrain-png)
  (make-instance
   'glhelp::gl-texture
   :handle
   (prog1
       (glhelp:pic-texture
	terrain-png
	:rgba)
     (glhelp:apply-tex-params
      (quote ((:texture-min-filter . :nearest)
	      (:texture-mag-filter . :nearest)
	      (:texture-wrap-s . :repeat)
	      (:texture-wrap-t . :repeat)))))))
(deflazy grass-png ()
  (load-png 
   (filesystem-util:rebase-path #P"grasscolor.png" *ourdir*)))
(deflazy blockshader (blockshader-text)
  (glhelp::create-gl-program blockshader-text))

(defun use-sandbox ()
  (let ((item 'atick))
    (unless (member item *trampoline*)
      (push 'atick *trampoline*))))

;;;; run use-sandbox before running funfair::main

;;example::
#+nil
(progn
  (ql:quickload :sandbox-funfair)
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
