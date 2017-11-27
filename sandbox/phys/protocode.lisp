
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

#+nil
(defun generate-fist-suite (fist-aabb fun)
  (let ((ansx nil)
	(ansy nil)
	(ansz nil)
	(exists? nil)
	(collect-fun nil)
	(exit (gensym)))
    (labels ((collect (x y z aabb)
	       (multiple-value-bind (first? is-minimum?)
		   (funcall collect-fun x y z aabb)
		 (declare (ignorable first? is-minimum?))       
		 (when (and is-minimum? first?)
		   (setf exists? t)
		   (setq ansx x
			 ansy y
			 ansz z)
		   (throw exit nil)))))
      (let ((derived-fun (funcall fun #'collect)))
	(values
	 (lambda ()
	   (values exists? ansx ansy ansz))
	 (lambda ()
	   (setf exists? nil))
	 (configure-collision-handler
	  (lambda (&key collect set-aabb set-exit)
	    (setf collect-fun collect)
	    (funcall set-aabb fist-aabb)
	    (funcall set-exit exit)
	    derived-fun)))))))


#+nil
(lambda (collect)
  (lambda (x y z)
    (unless (zerop (world:getblock x y z))
      (funcall collect x y z *block-aabb*))))

#+nil
(defun punch-func (px py pz vx vy vz)
  (let ((tot-min 2)
	(type :nothing)
	(ansx nil)
	(ansy nil)
	(ansz nil))
     (aabb-collect-blocks px py pz vx vy vz fist-aabb
			  (lambda (x y z)
			    (unless (zerop (world:getblock x y z))
			      (multiple-value-bind (minimum contact-type)
				  (aabbcc:aabb-collide
				   fist-aabb
				   px py pz
				   block-aabb
				   x y z
				   vx vy vz)
				(when (< minimum tot-min)
				  (setq tot-min minimum
					type contact-type
					ansx x
					ansy y
					ansz z))))))
     (values (if (= 2 tot-min) nil tot-min) type ansx ansy ansz)))

#+nil
(defparameter fist-side nil)

#+nil

(defparameter *fist-function* (constantly nil))

#+nil
(progno
 (defparameter *world-fist-collision-fun* nil)
 (defparameter *world-fist-collision-fun-reset* nil)
 (defparameter *world-fist-collision-fun-flush* nil)
 (setf (values
	*world-fist-collision-fun-flush*
	*world-fist-collision-fun-reset*
	*world-fist-collision-fun*)
       )

 (defparameter *selected-block* (vector 0 0 0))
 (defparameter *normal-block* (vector 0 0 0))
 (defparameter fist? nil)
 (defparameter *fist-position* (vector 0 0 0)))

#+nil
(configure-collision-handler
   (lambda (&key collect set-aabb &allow-other-keys)
     (funcall set-aabb *player-aabb*)
     (lambda (x y z)
       (when (aref mc-blocks::iscollidable (world:getblock x y z))
	 (funcall collect x y z *block-aabb*)))))

#+nil
(progno
 (defun make-collision-suite (&key
				(aabb nil)
				(test (lambda (x y z)
					(declare (ignore x y z)))))
   (declare (type (function (fixnum fixnum fixnum)) test))
   (let ((px 0.0)
	 (py 0.0)
	 (pz 0.0)
	 (vx 0.0)
	 (vy 0.0)
	 (vz 0.0)
	 (early-exit nil))
     (let ((taco (make-touch-collector)))
       (labels
	   ((set-aabb (new-aabb);;;
	      (setf aabb new-aabb))
	    (set-exit (exit);;;
	      (setf early-exit exit))
	    (set-test (new-test);;;
	      (setf test new-test))
	    (head (dpx dpy dpz dvx dvy dvz)
	      (setf (values px py pz vx vy vz)
		    (values dpx dpy dpz dvx dvy dvz)))
	    (reset ()
	      (reset-touch-collector taco))
	    (collect (foox fooy fooz fooaabb);;;
	      (multiple-value-bind (minimum type)
		  (aabbcc:aabb-collide
		   aabb
		   px py pz
		   fooaabb
		   foox fooy fooz
		   vx vy vz)
		(collect-touch minimum type taco)))
	    (tail ()
	      (catch early-exit
		(aabb-collect-blocks
		 px py pz vx vy vz aabb
		 test))
	      (multiple-value-bind (xclamp yclamp zclamp)
		  (collapse-touch vx vy vz taco)
		(values
		 (touch-collector-min-ratio taco)
		 xclamp yclamp zclamp)))
	    (full (px py pz vx vy vz);;;
	      (head px py pz vx vy vz)
	      (reset)
	      (tail)))
	 (list 'set-aabb #'set-aabb
	       'set-exit #'set-exit
	       'set-test #'set-test
	       'collect #'collect
	       'full #'full)))))
 (defun configure-collision-handler
     (fun &optional (data (make-collision-suite)))
   (let ((set-test (getf data 'set-test))
	 (full (getf data 'full))
	 (collect (getf data 'collect))
	 (set-aabb (getf data 'set-aabb))
	 (set-exit (getf data 'set-exit)))
     (declare (type (function (number number number aabbcc:aabb)
			      (values single-float symbol))
		    collect))
     (funcall
      set-test
      (funcall fun
	       :collect collect
	       :set-aabb set-aabb
	       :set-exit set-exit))
     full)))
