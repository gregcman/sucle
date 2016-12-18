;;;; aabbcc.lisp

(in-package #:aabbcc)

;;;below is code that I thought super hard about
;;;I hope it gives u a hard on

(defun step-motion (get-collision-data px py pz vx vy vz &optional xclamp yclamp zclamp (depth 0))
  (multiple-value-bind (ratio xc yc zc)
      (funcall get-collision-data px py pz vx vy vz)
    (multiple-value-bind (newvx newvy newvz)
	(clamp-vec vx vy vz xc yc zc)
      (if (or (> depth 5)
	      (and (zerop vx)
		   (zerop vy)
		   (zerop vz)))
	  (values px py pz xclamp yclamp zclamp xc yc zc)
	  (let ((whats-left (- 1 ratio)))
	    (step-motion
	     get-collision-data
	     (+ px (* ratio newvx))
	     (+ py (* ratio newvy))
	     (+ pz (* ratio newvz))
	     (* newvx whats-left)
	     (* newvy whats-left)
	     (* newvz whats-left)
	     (or xclamp xc)
	     (or yclamp yc)
	     (or zclamp zc)
	     (1+ depth)))))))

(defun clamp-vec (vx vy vz xclamp yclamp zclamp)
  (values
   (if xclamp 0 vx)
   (if yclamp 0 vy)
   (if zclamp 0 vz)))

(defun collapsecollisions (places)
  (let ((min-dist 1)
	(xclamp nil)
	(yclamp nil)
	(zclamp nil))
    (dolist (x places)
      (if (= (car x) min-dist)
	  (progn
	    (setf xclamp (or xclamp (second x)))
	    (setf yclamp (or yclamp (third x)))
	    (setf zclamp (or zclamp (fourth x))))
	  (if (< (car x) min-dist)
	      (progn
		(setf min-dist (car x))
		(setf xclamp (second x))
		(setf yclamp (third x))
		(setf zclamp (fourth x))))))
    (values min-dist xclamp yclamp zclamp)))

(defun aabb-collide (aabb1 px py pz aabb2 pos2 vx vy vz)
  (%%collide (+ px (aabb-minx aabb1)) (+ py (aabb-miny aabb1)) (+ pz (aabb-minz aabb1))
	     (+ px (aabb-maxx aabb1)) (+ py (aabb-maxy aabb1)) (+ pz (aabb-maxz aabb1))	    
	     vx vy vz
	     (+ (elt pos2 0) (aabb-minx aabb2))
	     (+ (elt pos2 1) (aabb-miny aabb2))
	     (+ (elt pos2 2) (aabb-minz aabb2))
	     (+ (elt pos2 0) (aabb-maxx aabb2))
	     (+ (elt pos2 1) (aabb-maxy aabb2))
	     (+ (elt pos2 2) (aabb-maxz aabb2))))

(defstruct aabb
  minx
  miny
  minz
  maxx
  maxy
  maxz)

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

(defun corner-case (ddx ddy ddz)
  (let ((dx (abs ddx))
	(dy (abs ddy))
	(dz (abs ddz)))
    (if (= dx dy)
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
			)))))))

(defun %%collide (ax0 ay0 az0 ax1 ay1 az1 dx dy dz bx0 by0 bz0 bx1 by1 bz1)
  (multiple-value-bind (x y z)
      (%collide ax0 ay0 az0 ax1 ay1 az1 dx dy dz bx0 by0 bz0 bx1 by1 bz1)
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
	(if xt
	    (if yt
		(if zt
		    (multiple-value-bind (afoo bfoo cfoo)
			(corner-case dx dy dz)
		      (values minimum afoo bfoo cfoo)) ;;corner xyz
		    (if (= dx dy)
			(values minimum t t nil)
			(if (> dx dy)
			    (values minimum nil t nil)
			    (values minimum t nil nil))) ;;edge xy
		    )
		(if zt
		    (if (= dx dz)
			(values minimum t nil t)
			(if (> dx dz)
			    (values minimum nil nil t)
			    (values minimum t nil nil))) ;;edge xz
		    (values minimum t nil nil) ;;face x
		    ))
	    (if yt
		(if zt
		    (if (= dy dz)
			(values minimum nil t t)
			(if (> dy dz)
			    (values minimum nil nil t)
			    (values minimum nil t nil)))
		    (values minimum nil t nil) ;;face y
		    )
		(if zt
		    (values minimum nil nil t) ;;face z
		    (values minimum nil nil nil) ;;no collision
		    )))))))

(defun testit (&rest args)
  (multiple-value-bind (a b c d)
      (apply #'%%collide args)
    (print (list a b c d))))

(defun testit2 (&rest args)
  (multiple-value-bind (a b c d)
      (apply #'%%%collide args)
    (print (list a b c d))))

(defun %%%collide (ax0 ay0 az0 ax1 ay1 az1 dx dy dz bx0 by0 bz0 bx1 by1 bz1)
  (%%collide ax0 ay0 az0
	     (+ ax1 ax0) (+ ay1 ay0) (+ az1 az0)
	     dx dy dz
	     bx0 by0 bz0
	     (+ bx1 bx0) (+ by1 by0) (+ bz1 bz0)))
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

(defun %collide (ax0 ay0 az0 ax1 ay1 az1 dx dy dz bx0 by0 bz0 bx1 by1 bz1)
  ;;if the velocity is zero, there is no point to test it
  ;;if it is nonzero, we check the direction
  ;; if the direction is positive but surface 2 is in the
  ;;negative direction relative to surface 1 we discard
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

(defun r-intersect (ax0 ay0 ax1 ay1 bx0 by0 bx1 by1)
  "0 means the edges touch. positive number means there is space between
negative means it is past. the symbols u r l and b represent the top,
right, left, and bottom of the first rectangle."
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
