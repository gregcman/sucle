;;;; aabbcc.lisp

(in-package #:aabbcc)

;;; "aabbcc" goes here. Hacks and glory await!

(defun collapsecollisions (places)
  (let ((collisions (make-array 6 :initial-contents '(1 1 1 1 1 1))))
    (dolist (boxdata places)
      (dotimes (val 6)
	(let ((info (elt boxdata val)))
	  (if (numberp info)
	      (let ((nowval (elt collisions val)))
		(if (numberp nowval)
		    (if (< info nowval)
			(setf (elt collisions val) info)))))
	  (if (eq :contact info)
	      (setf (aref collisions val) :contact)))))
    collisions))

(defun clamp-vec (vec collisiondata)
  (let ((newvec (make-array 3 :initial-contents vec)))
    ;;clamp to contacts
    (dotimes (n 6)
      (let* ((place (bleck n))
	     (colval (elt collisiondata n)))
	(if (eq colval :contact)
	    (let ((wowz (if (oddp n) 1 -1)))
	      (if (< 0 (* (elt newvec place) wowz))
		  (setf (aref newvec place) 0))))))
    newvec))

(defun smallscalevec (collisiondata)
  (let ((smallest 2))
    ;;get the smallest distance
    (dotimes (n 6)
      (let* ((colval (elt collisiondata n)))
	(if (numberp colval)
	    (if (> smallest colval)
		(setf smallest colval)))))
    smallest))

(defstruct aabb
  (minx -0.5)
  (miny -0.5)
  (minz -0.5)
  (maxx 0.5)
  (maxy 0.5)
  (maxz 0.5))

(defun block-aabb ()
  (make-aabb))

(defun player-aabb ()
  (make-aabb
   :minx -0.3
   :miny 0
   :minz -0.3
   :maxx 0.3
   :maxy 1.62
   :maxz 0.3))

(defstruct rectangle
  minx
  miny
  maxx
  maxy
  z)

(defun rect-intersect (a b)
  (if (and (< (rectangle-minx a) (rectangle-maxx b))
	   (< (rectangle-minx b) (rectangle-maxx a))
	   (< (rectangle-miny a) (rectangle-maxy b))
	   (< (rectangle-miny b) (rectangle-maxy a)))
      t
      nil))

(defun translate-rect (rect vec2)
  (incf (rectangle-minx rect) (elt vec2 0))
  (incf (rectangle-maxx rect) (elt vec2 0))
  (incf (rectangle-miny rect) (elt vec2 1))
  (incf (rectangle-maxy rect) (elt vec2 1))
  rect)

(defun my-helper-func (x)
  (if (evenp x)
      (1+ x)
      (1- x)))

(defun bleck (x)
  (floor (/ x 2)))

(defun some-rect (vec4 num)
  (make-rectangle
   :minx (elt vec4 0)
   :maxx (elt vec4 1)
   :miny (elt vec4 2)
   :maxy (elt vec4 3)
   :z num))

(defun aabb-to-rects (abba pos)
  (let ((rectlist
	 (vector
	  (+ (elt pos 0) (aabb-minx abba))
	  (+ (elt pos 0) (aabb-maxx abba))
	  (+ (elt pos 1) (aabb-miny abba))
	  (+ (elt pos 1) (aabb-maxy abba))
	  (+ (elt pos 2) (aabb-minz abba))
	  (+ (elt pos 2) (aabb-maxz abba))))
	(shitlist nil))
    (dotimes (x 6)      
      (let* ((alist (vector 0 1 2 3 4 5))
	     (another (remove (my-helper-func x)
			      (remove x alist)))
	     (arect (some-rect
		     (let ((sometin (make-array 4)))
		       (dotimes (n 4)
			 (setf (aref sometin n) (aref rectlist (aref another n))))
		       sometin)
		     (aref rectlist x))))
	(push 
	 (list arect (- (mod x 2) 0.5) x)
	 shitlist)))
    (reverse shitlist)))

(defun %aabb-intersect (aabb1 pos1 aabb2 pos2 vec3)
  (let ((onerects  (aabb-to-rects aabb1 pos1))
	(tworects  (aabb-to-rects aabb2 pos2))
	(somelist nil))
    (dotimes (x 6)
      (let* ((dat1 (elt onerects x))
	     (dat2 (elt tworects (my-helper-func x)))
	     (therect
	      (%rect-intersect
	       (first dat1)
	       (first dat2)
	       (wtf x vec3)
	       (second dat1)
	       (second dat2))))
	(push therect somelist)))
    (reverse somelist)))

(defun arect (x y x1 y1)
  (make-rectangle :minx x :miny y :maxx x1 :maxy y1))

(defun arect2 (x y x1 y1 z)
  (make-rectangle :minx x :miny y :maxx x1 :maxy y1 :z z))

(defun tests ()
  (print (%rect-intersect
	  (arect2 0 0 1 1 -1)
	  (arect2 0 0 1 1 1)
	  (vector 0 0 5) 1 -1))
  (print (%rect-intersect
	  (arect2 0 0 1 1 -1)
	  (arect2 0 1 1 2 1)
	  (vector 0 0 5) 1 -1))
  (print (%rect-intersect
	  (arect2 0 0 1 1 -1)
	  (arect2 0 0 1 1 1)
	  (vector 0 0 5) 1 -1))
  (print (%rect-intersect
	 (arect2 0 0 1 1 -1)
	 (arect2 0 0 1 1 1)
	 (vector 0 0 5) 1 -1)))

(defun %rect-intersect (a b vec3 adir bdir)
  (let* ((aiz (rectangle-z a))
	 (biz (rectangle-z b))
	 (diff (- biz aiz)))
    (let ((zdelt (elt vec3 2)))
      (if (zerop zdelt)
	  ;;the rectangles do not move in the z direction
	  (if (zerop diff)
	      ;;the rectangles are on the same plane
	      (if (rect-intersect
		   (translate-rect a vec3)
		   b)
		  ;;contact established after moving,
		  ;;sliding contact but no collision
		  :contact
		  ;;nocontact after moving
		  :non)
	      ;;the rectangles are not on the same plane
	      ;;and neither move so its no
	      :no)
	  ;;the rectangles move in the z direction
	  (if (or
	       (and
		(<= diff 0)
		(< adir 0)
		(< 0 bdir)
		(< zdelt 0))
	       (and
		(<= 0 diff)
		(< 0 adir)
		(< bdir 0)
		(< 0 zdelt)))
	      ;;the rectangles do intersect becuase
	      ;;the directions line up correctly
	      (let* ((ratio (/ diff zdelt))
		     (transx (* ratio (elt vec3 0)))
		     (transy (* ratio (elt vec3 1)))
		     (moveda (translate-rect
			      a
			      (vector transx transy))))
		(if (rect-intersect
		     moveda
		     b)
		    (if (zerop ratio)
			;;the rectangles were on the same plane
			;;to begin with, if they intersect its
			;;definitely a contact
			:contact
			;;the rectangles were not on the same
			;;plane and they definitely intersect
			ratio)
		    (if (not (edgy-rect-intersect moveda b))
			;;not even the corners or the
			;;edges are touching in this case.
			;;absolutely nothing.
			:nope
			;;this is the part that gets hairy,
			;;as it involves the direction of movement
			;;to decide whether its contact or not
			(nasty-intersection a b vec3))))
	      ;;the rectangles do not intersect by the
	      ;;nature of the directions they are facing
	      (progn
		:ehh))))))

;;if there is a length an edge touches, if the rectangle is moving
;;towards the other rectangle then contact depends on whether speed in the
;;direction perpendicular to the edge is greater or equal to
;;the speed into the page
(defun nasty-intersection (a b vec3))

(defun edgy-rect-intersect (a b)
  (if (and (<= (rectangle-minx a) (rectangle-maxx b))
	   (<= (rectangle-minx b) (rectangle-maxx a))
	   (<= (rectangle-miny a) (rectangle-maxy b))
	   (<= (rectangle-miny b) (rectangle-maxy a)))
      t
      nil))

(defun wtf (x vec3)
  (let* ((ouch (floor (/ x 2)))
	 (eek (elt vec3 ouch))
	 (umm (delete-at vec3 ouch))
	 (dayum (concatenate 'vector umm (vector eek))))
    dayum))

(defun unwtf (x vec3)
  (let* ((ouch (floor (/ x 2)))
	 (myval (elt vec3 2)))
    (subseq
     (insert-at myval vec3 ouch)
     0
     3)))
;;wtf tests (dotimes (x 6) (print (unwtf x (print (wtf x (vector :wot :teh :fack))))))

(defun insert-at (num vec place)
  (let* ((start (subseq vec 0 place))
	 (end (subseq vec place (length vec))))
    (concatenate 'vector start (vector num) end)))

(defun delete-at (vec place)
  (let* ((start (subseq vec 0 place))
	 (end (subseq vec (1+ place) (length vec))))
    (concatenate 'vector start end)))

(defun vecscale (vec3 scale)
  (vector
   (* scale (elt vec3 0))
   (* scale (elt vec3 1))
   (* scale (elt vec3 2))))

(defun vecsubtract (vec3 vec32)
  (vector
   (- (elt vec3 0) (elt vec32 0))
   (- (elt vec3 1) (elt vec32 1))
   (- (elt vec3 2) (elt vec32 2))))

(defun vecadd (vec3 vec32)
  (vector
   (+ (elt vec3 0) (elt vec32 0))
   (+ (elt vec3 1) (elt vec32 1))
   (+ (elt vec3 2) (elt vec32 2))))

(defun veczerop (vec)
  (dotimes (n (length vec))
    (if (zerop (elt vec n))
	nil
	(return-from veczerop nil)))
  t)

(defun finish-clamps (aabbgetfunc vec3position vec3velocity &optional
						  (depth 0) (collisionacc (vector 0 0 0 0 0 0)))
  (let* ((collisiondata (funcall aabbgetfunc vec3position vec3velocity))
	 (next (clamp-vec vec3velocity collisiondata))
	 (scalez (smallscalevec collisiondata)))  
    (let ((coldone t))
      (dotimes (x 6)
	(if (not (if (numberp (elt collisiondata x))
		     (or (= 1 (elt collisiondata x))
			 (zerop (elt collisiondata x)))
		     (eq :contact (elt collisiondata x))))
	    (setf coldone nil)))
      (dotimes (n 6)
	(if (eq :contact (elt collisiondata n))
	    (setf (aref collisionacc n) :contact)))
      (if (and
	   ( > depth 0)
	   (veczerop vec3velocity)
	   coldone)
	  (progn	   
	    (values
	     vec3position
	     collisiondata
	     collisionacc))
	  (finish-clamps
	   aabbgetfunc
	   (vecadd
	    vec3position
	    (vecscale next scalez))
	   (vecscale next (- 1 scalez))
	   (incf depth)
	   collisionacc)))))
