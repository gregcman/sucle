(in-package :sandbox)

(defparameter onground nil)
(defparameter cameraVelocity (mat:onebyfour '(0.0 0.0 0.0 0)))
(defparameter wow nil)

(defun physics (camera)
  "a messy function for the bare bones physics"
  (setf wow camera)
  (let ((wowzer nil)
	(collisiondata nil)
	(velclamp nil))
    (multiple-value-bind (a b c)
	(finish-clamps
	 (mat-vec (simplecam-pos camera))
	 (mat-vec cameraVelocity))
      (setf wowzer a)
      (setf collisiondata b)
      (setf velclamp c))
    (setf onground (if (numberp (elt collisiondata 2))
		       (= 0 (elt collisiondata 2))
		       (eq :contact (elt collisiondata 2))))
    
    (let* ((newpos (mat:add (simplecam-pos camera) cameraVelocity))
	   (blockid (mat-pos newpos)))

      (setf (simplecam-pos camera)
	    (vec-mat
	     wowzer))
      (setf cameraVelocity
	    (mat-clamper cameraVelocity velclamp))
      
      (if (not onground)
	  (mat:add! cameraVelocity (mat:onebyfour (list 0 (* -0.08 (expt tickscale 2)) 0 0))))
      (let ((airscaled (mat:onebyfour (list
				       (row-major-aref cameraVelocity 0)
				       0
				       (row-major-aref cameraVelocity 2)
				       0))))
	(mat:scale! airscaled (* 0.6 0.91 0.5))
	(setf (row-major-aref cameraVelocity 0) (row-major-aref airscaled 0))
	(setf (row-major-aref cameraVelocity 2) (row-major-aref airscaled 2)))
      (setf (row-major-aref cameraVelocity 1)
	    (* (expt 0.98 tickscale)
	       (row-major-aref cameraVelocity 1)))

      (progn 
	(if (not (= 0 blockid))
	    (progn
	      (setf (row-major-aref (simplecam-pos camera) 1)
		    (- (ceiling (row-major-aref (simplecam-pos camera) 1)) 0.5))
	      (setf (row-major-aref cameraVelocity 1) 0))))
      (if (> 0 (row-major-aref (simplecam-pos camera) 1))
	  (progn	  
	    (setf (row-major-aref cameraVelocity 1) 0)
	    (setf (row-major-aref (simplecam-pos camera) 1) 0)
	    (setf (simplecam-pos camera) (mat:onebyfour '(0 128 0 1))))))))

(defun mat-clamper (mat coldata)
  (vec-mat
   (clamp-vec (mat-vec mat) coldata)))

(defun vec-mat (vec)
  (let ((newmat (mat:onebyfour '(0 0 0 0))))
    (dotimes (x (length vec))
      (setf (row-major-aref newmat x) (elt vec x)))
    newmat))

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

(defun finish-clamps (vec3position vec3velocity &optional
						  (depth 0) (collisionacc (vector 0 0 0 0 0 0)))
  (let* ((collisiondata (get-blocks-around-player vec3position vec3velocity))
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
					; (print (list vec3velocity next scalez collisiondata depth))
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
	   (vecadd
	    vec3position
	    (vecscale next scalez))
	   (vecscale next (- 1 scalez))
	   (incf depth)
	   collisionacc)))))

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

(defun get-blocks-around-player (vec3player vel)
  (let ((places nil))
    (dotimes (x 3)
      (dotimes (y 4)
	(dotimes (z 3)
	  (let ((blockx (round (1- (+ x (elt vec3player 0)))))
		(blocky (round (1- (ceiling (+ y (elt vec3player 1))))))
		(blockz (round (1- (+ z (elt vec3player 2))))))
	    (let ((blockid (mat-pos (vector blockx blocky blockz))))
	      (if (not (zerop blockid))
		  (let ((vec
			 (%aabb-intersect
			  (player-aabb)
			  vec3player
			  (block-aabb)
			  (vector blockx blocky blockz)
			  vel)))
		    (let ((stuffs (remove nil vec)))
		      (if stuffs
			  (push
			   stuffs
			   places))))))))))
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
      collisions)))

(defun mat-vec (mat)
  (vector
   (row-major-aref mat 0)
   (row-major-aref mat 1)
   (row-major-aref mat 2)))

(defun mat-pos (mat)
  (getblock
   (round (row-major-aref mat 0))
   (round (row-major-aref mat 1))
   (round (row-major-aref mat 2))))

(defun controls (camera)
  "mice look and keys move"
  (mouse-looking camera)
  (mat:add!
   cameraVelocity
   (keymovement camera))

  (in:p+1 3 (lambda () (aplatform
			(mat-world-pos (simplecam-pos camera))
			(random 97))))
  (in:p+1 2 (lambda () (notaplatform (mat-world-pos (simplecam-pos camera))))))

(defun mat-world-pos (mat)
  (vector
   (round (row-major-aref mat 0))
   (round (row-major-aref mat 1))
   (round (row-major-aref mat 2))))

(defun mouse-looking (camera)
  (let* ((change (in:delta))
	 (x (* 1/360 (aref change 0)))
	 (y (* 1/360 (aref change 1))))
    (setf
     (simplecam-yaw camera)
     (mod (+ (simplecam-yaw camera) x) (* 2 pi)))
    (setf (simplecam-pitch camera)
	  (anothershit
	   (+ (simplecam-pitch camera) y) (/ pi 2)))))

(defun anothershit (x whatthefuck)
  "used to clamp the pitch"
  (if (> x whatthefuck)
      whatthefuck
      (if (< x (- whatthefuck))
	  (- whatthefuck)
	  x)))

(defun good-func (some-number)
  "maps keys to vectors"
  (lambda (x)
    (if (in:key-p (first x))
	(mat:add! some-number
		  (mat:onebyfour (second x))))))

(defun empty-vec4 ()
  (mat:onebyfour '(0 0 0 0)))

(defun key-legs ()
  "keys for walking"
  (let* ((delta (empty-vec4))
	 (lemod (good-func delta)))
    (mapcar
     lemod
     '((#\d ( 1  0  0  0))
       (#\e (-1  0  0  0))
       (#\s ( 0  0  1  0))
       (#\f ( 0  0 -1  0))))
    (mat:scale! (mat:normalize! delta) (* 0.7 (expt tickscale 2)))
    delta))

(defun key-jumps ()
  "keys for jumping"
  (let* ((delta (empty-vec4))
	 (lemod (good-func delta)))
    (if onground
	(mapcar
	 lemod
	 `((#\Space (0 ,(* 0.42 (expt tickscale 1)) 0 0)))))
    delta))

(defun keymovement (camera)
  "total keymovement"
  (mat:mmul! (mat:add (key-legs) (key-jumps))
	     (mat:rotation-matrix 0 1 0
				  (simplecam-yaw camera))))


(defun seed (times val rad)
  (dotimes (n times)
    (setblock
     (+ 0 (random rad))
     (+ 64 (random rad))
     (+ 0 (random rad))
     val)))

(defun aplatform (pos blockid)
  (let ((i (elt pos 0))
	(j (elt pos 1))
	(k (elt pos 2)))
    (dotimes (a 3)
      (dotimes (b 3)
	(setblock-with-update (+ a i -1) (- j 1) (+ b k -1) blockid)))))

(defun notaplatform (pos)
  (let ((i (elt pos 0))
	(j (elt pos 1))
	(k (elt pos 2)))
    (dotimes (a 3)
      (dotimes (b 3)
	(setblock-with-update (+ a i -1) (+ j) (+ b k -1) 0)))))

(defun yay (x)
  (- (random (+ x x)) x))

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

(defun %rect-intersect (a b vec3 adir bdir)
  (let* ((aiz (rectangle-z a))
	 (biz (rectangle-z b))
	 (diff (- biz aiz)))
    (let ((zdelt (elt vec3 2)))
      (if (zerop zdelt)
	  (if (zerop diff)
	      (if (rect-intersect
		   (translate-rect a vec3)
		   b)
		  :contact
		  :non)
	      :no)
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
			:contact
			ratio)
		    :nope))
	      (progn
		:ehh))))))

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
