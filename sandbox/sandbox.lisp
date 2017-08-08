(in-package #:sandbox)

(defparameter *gl-thread* nil)
(defun threaded-main (func)
  (unless (and *gl-thread*
	       (bordeaux-threads:thread-alive-p *gl-thread*))
    (setf *gl-thread*
	  (bordeaux-threads:make-thread
	   (lambda ()
	     (window:wrapper func))))))

(defun qwer (func)
  (window:wrapper func))

;;(defparameter init-hook (hook:create-hook))
(defun handoff-three ()
  (initialization1)
  (injection))

(defun initialization1 ()
  (clrhash *g/call-list*)
  (clrhash *g/texture*)
  (clrhash *g/shader*)
  (clrhash *g/chunk-call-list*)
  
  (glinnit) ;opengl
  (physinnit) ;physics
  )

(defun thunkit ()
  (physics)
  (set-render-cam-pos *camera*)
  (remove-spurious-mouse-input)
  (render)
  (incf *ticks*))

(defun injection ()
  (window:poll)
  (thunkit)
  (window:update-display)
  (unless window:*status* 
    (injection)))

(defparameter *ticks* 0)
(defun main ()
  (threaded-main #'handoff-three))


(defun spill-hash (hash)
  (loop for key being the hash-keys of hash
     using (hash-value value)
     do (format t "~S ~S~%" key value)))

(defun int-scale (int scale)
  (truncate (* int scale)))

(defun fun-setup ()
  (test-world)
  (erase-bottom))
(progno
 (color-grasses)
 (goto 16 80 -16))

(defun erase-bottom ()
  (dobox ((x 0 128) (y 0 64) (z -128 0))
	 (plain-setblock x y z 0 0)))

(defun test-world ()
  (dobox ((x 0 8) (y -4 4))
	 (someseq x y)))

(defun spawn ()
  (goto 64 80 -64))

(defun goto (x y z)
  (setf *xpos* x
	*ypos* y
	*zpos* z))

(defun color-grasses ()
  (get-image "terrain.png")
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

(progno #(113 174 70 255)  #(198 304 122 255))
;;;grass is 0 240
;;;leaves is [64 80] 192
(defun modify-greens (xpos ypos &optional (color
					   (case 1
					     (0 #(1742848/8775 2673664/8775 1079296/8775 255))
					     (1 (imagewise:getapixel 0 255 (get-image "misc/grasscolor.png")))
					     (2 (imagewise:getapixel 0 0 (get-image "misc/grasscolor.png")))
					     (3 (imagewise:getapixel 255 0 (get-image "misc/grasscolor.png"))))
					   ) (terrain (get-image "terrain.png")))
  (dobox ((x xpos (+ 16 xpos)) (y ypos (+ 16 ypos)))
	 (multiply-into (imagewise:getapixel y x terrain) color)))

(defun reload-grasses ()
  (remhash "terrain.png" *g/image*)
  (color-grasses)
  (remhash :terrain *g/texture*))

(defun shit (x)
  (print x global-output))

(defun force-quit ()
  (when ourthread
    (SB-THREAD:terminate-thread (sb-thread:terminate-thread ourthread))))

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
  (multiple-value-bind (i j k) (step-next x y z dx dy dz)
       (multiple-value-bind (value i? j? k?) (smallest i j k)
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
       (multiple-value-bind (ratio newx newy newz i? j? k?) (aux-step-next x y z dx dy dz)
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

(defun test (a b)
  (multiple-value-bind (c d) (extract-polar-coords (unit-pitch-yaw (coerce (deg-rad a) 'single-float)
								   (coerce (deg-rad b) 'single-float)))
    (values (rad-deg c)
	    (rad-deg d))))

(defun test2 (dx dy)
  (multiple-value-bind (y p) (new-direction
			      (coerce dx 'single-float)
			      (coerce dy 'single-float)
			      (coerce (/  pi 180) 'single-float))
    (values (rad-deg y) (rad-deg p))))

(defparameter png-resources
  '(""
    "terrain.png"
    "pack.png" "particles.png" 
    ("achievement/"
     "bg.png" "icons.png")
    ("armor/"
     "chain_1.png" "chain_2.png" "cloth_1.png" "cloth_2.png"
     "diamond_1.png" "diamond_2.png"
     "gold_1.png" "gold_2.png"
     "iron_1.png" "iron_2.png" "power.png")
    ("art/"
     "kz.png")
    ("environment/"
     "clouds.png" "rain.png" "snow.png")
    ("font/"
     "default.png")
    ("gui/"
     "background.png" "container.png"
     "crafting.png" "furnace.png" "gui.png" "icons.png"
     "inventory.png"
     "items.png" "logo.png" "particles.png" "slot.png"
     "trap.png" "unknown_pack.png")
    ("item/"
     "arrows.png" "boat.png" "cart.png" "door.png" "sign.png")
    ("misc/"
     "dial.png" "foliagecolor.png" "footprint.png"
     "grasscolor.png"
     "mapbg.png" "mapicons.png" "pumpkinblur.png"
     "shadow.png" "vignette.png" "water.png" "watercolor.png")
    ("mob/"
     "char.png" "chicken.png" "cow.png"  "creeper.png"
     "ghast.png" "ghast_fire.png"
     "pig.png" "pigman.png" "pigzombie.png"
     "saddle.png" "sheep.png" "sheep_fur.png" "silverfish.png"
     "skeleton.png"
     "slime.png" "spider.png" "spider_eyes.png"
     "squid.png" "wolf.png" "wolf_angry.png" "wolf_tame.png"
     "zombie.png")
    ("terrain/"
     "moon.png" "sun.png")
    ("title/"
     "black.png" "mclogo.png" "mojang.png")
    ("skybox/"
     "cheap.png")))

(defparameter simple-resources
  '(""
    "terrain.png"
    ("terrain/"
     "moon.png" "sun.png")
    ("skybox/"
     "cheap.png")
    ("misc/"
     "grasscolor.png")
    ("gui/"
     "gui.png")))

(defun load-file-images (path-list start-dir)
  (dolist (path path-list)
    (let ((filepath (merge-pathnames path start-dir)))
      (load-file-image filepath (namestring path)))))

(defun load-file-text (path text-name)
  (let ((text (file-string path)))
    (set-text text-name text)
    text))

(defun load-file-image (path image-name)
  (let ((png (imagewise:load-png path)))
    (imagewise:flip-image png)
    (set-image image-name png)
    png))


(defun hash-symbol (symbol)
  (declare (type symbol symbol))
  (sxhash symbol))

(defun funcall-test (func)
  (declare (type (function () (values)) func))
  (funcall func))


(defparameter foob
  `(progn
     (declaim (inline floop))
     (defun floop ()
       (print 345))
     (declaim (notinline floop))
     (values #'floop)))

(declaim (inline test234))
(defun test234 ()
  (floop)
  (floop)
  (floop))
(declaim (notinline test234))


;;;;sbcl does retarded checks when one wants to set a symbol's value cell
;;;;so this function bypasses it
;;;;setvq means "set VALUE quoted"
(declaim (inline setvq))
(defun setvq (symbol new)
  #+sbcl (sb-kernel:%set-symbol-global-value symbol new)
  #-sbcl (set symbol new))

(defun wtf3 (symbol new)
  (sb-kernel:%set-symbol-global-value symbol new))
(defun test234other ()
  (locally (declare (inline floop test234))
    (test234)
    (floop)
    (floop))
  (floop))

(defun set5 (symbol)
  (declare (type symbol symbol)
	   (optimize (speed 3) (safety 0)))
  (sb-impl::%set-symbol-value symbol 5))

(defstruct astruct
  symbol)

(declaim (inline ssetvq))
(defun ssetvq (astruct new)
  (setf (astruct-symbol astruct) new))

(defun wtf? (symbol)
  (declare (type symbol symbol))
  (let ((a symbol))
    (dotimes (X 10000000) (setvq a x)) a))

(defparameter wof nil)
(defun wtf2? ()
  (dotimes (X 10000000) (set 'wop x)))

(defun test56 (a b)
  (declare (type fixnum a b))
  (let ((c (+ a b)))
    (declare (type fixnum c))
    c))

(defun make-cons (num)
  (cons num (sxhash num)))

(defun make-cons-hash ()
  (make-hash-table :test 'eq :hash-function #'cdr))


(defun wtf (num)
  (declare (type fixnum num)
	   
	   (optimize (speed 3) (safety 0)))
  (let ((a 123))
    (declare (type (unsigned-byte 64) a))
    (the fixnum (* a num))))


(declaim (type list *id-free-list*))
(defparameter *id-free-list* (list nil))

(defun swap (a b vector)
  (rotatef (aref vector a) (aref vector b)))

(defun scramblehash (hash)
  (let ((keys (make-array 0 :adjustable t :fill-pointer 0))
	(values (make-array 0 :adjustable t :fill-pointer 0)))
    (maphash (lambda (k v) (vector-push-extend k keys) (vector-push-extend v values)) hash)
    (let ((tot (length keys)))
      (dotimes (x (* tot 10))
	(swap (random tot) (random tot) keys)))
    (dotimes (x (length keys))
      (setf (gethash (vector-pop keys) hash) (vector-pop values)))))

(defmacro nif (test-form else if)
  `(if ,test-form
       ,if
       ,else))

(defun remove-empty-chunks ()
  (maphash (lambda (k v)
	     (when (all-zeroes-p v)
	       (remhash k world:chunkhash)))
	   world:chunkhash))

(defun wtf-increment ()
  (maphash (lambda (k v)
	     (nreverse v))
	   world:chunkhash))

(defun wtf-reverse ()
  (maphash (lambda (k v)
	     (map-into v #'1+ v))
	   world:chunkhash))

(defun wtf-reverse ()
  (maphash (lambda (k v)
	     (map-into v (lambda (x y) (max x y)) v (reverse v)))
	   world:chunkhash))

(defun wtf-reverse ()
  (maphash (lambda (k v)
	     (map-into v (lambda (x) (if (zerop x) 1 x)) v ))
	   world:chunkhash))

(defun wtf-fill ()
  (maphash (lambda (k v)
	     (fill v 1))
	   world:chunkhash))

(defun wtf-squared ()
  (maphash (lambda (k v)
	     (map-into v (lambda (x)
			   (let ((ans (mod (* x 2) 95)))
			     (if (aref mc-blocks::renderasnormalblock ans)
				 ans
				 1))) v))
	   world:chunkhash))

(defparameter height (make-array (list 128 128)))
(defparameter avg (make-array (list 128 128)))

(with-unsafe-speed
  (defun yo ()
    (let ((array height))
      (declare (type (simple-array t (128 128)) array))
      (dobox ((x 0 128)
	      (z -128 0))
	     (setf (aref array x (the fixnum (1- (- z))))
		   (floor (the fixnum
			       (+ (find-height x (the fixnum (1+ z)))
				  (find-height (the fixnum (1+ x)) z)
				  (find-height x (the fixnum (1- z)))
				  (find-height (the fixnum (1- x)) z)))
			  4))))))

(declaim (ftype (function (fixnum fixnum) fixnum) find-height))
(defun find-height (x y)
  (loop for i downfrom 256 to 0 do
       (let ((block (world:getblock x i y)))
	 (unless (zerop block)
	   (return-from find-height i))))
  0)

(defun all-zeroes-p (sequence)
  (dotimes (x (length sequence))
    (unless (zerop (aref sequence x))
      (return-from all-zeroes-p nil)))
  t)

(defun test= (a b)
  (declare (type symbol a b)
	   (optimize (speed 3) (safety 0)))
  (eq a b))

(defparameter *save* #P"terrarium2/")

(defparameter *saves-dir* (merge-pathnames #P"saves/" ourdir))

(defun save (filename &rest things)
  (let ((path (merge-pathnames filename *saves-dir*)))
    (with-open-file (stream path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (dolist (thing things)
	(print thing stream)))))

(defun save2 (thingfilename &rest things)
  (apply #'save (merge-pathnames (format nil "~s" thingfilename) *save*) things))

(defun savechunk (position)
  (let ((position-list (multiple-value-list (world:unhashfunc position))))
    (save2 position-list
	   (gethash position world:chunkhash)
	   (gethash position world:lighthash)
	   (gethash position world:skylighthash))))

(defun save-world ()
  (maphash (lambda (k v)
	     (declare (ignorable v))
	     (savechunk k))
	   world:chunkhash))

(defun looad-world ()
  (let ((files (uiop:directory-files (merge-pathnames *save* *saves-dir*))))
    (dolist (file files)
      (loadchunk (apply #'world:chunkhashfunc (read-from-string (pathname-name file)))))))

(defun myload2 (thingfilename)
  (myload (merge-pathnames (format nil "~s" thingfilename) *save*)))

(defun myload (filename)
  (let ((path (merge-pathnames filename *saves-dir*)))
    (let ((things nil))
      (with-open-file (stream path :direction :input :if-does-not-exist nil)
	(tagbody rep
	   (let ((thing (read stream nil nil)))
	     (when thing
	       (push thing things)
	       (go rep)))))
      (nreverse things))))

(defun loadchunk (position)
  (let ((position-list (multiple-value-list (world:unhashfunc position))))
    (let ((data (myload2 position-list)))
      (when data 
	(destructuring-bind (blocks light sky) data
	  (setf (gethash position world:chunkhash) (coerce blocks '(simple-array (unsigned-byte 8) (*))))
	  (setf (gethash position world:lighthash) (coerce light '(simple-array (unsigned-byte 4) (*))))
	  (setf (gethash position world:skylighthash) (coerce sky '(simple-array (unsigned-byte 4) (*)))))
	(return-from loadchunk t)))))

(defun simple-relight ()
  (dobox ((x 0 128)
	  (y 0 128)
	  (z -128 0))
	 (let ((blockid (world:getblock x y z)))
					;(unless (zerop blockid))
	   (let ((light (aref mc-blocks::lightvalue blockid)))
	     (if (zerop light)
		 (plain-setblock x y z blockid light 0)
		 (setblock-with-update x y z blockid light)))))
  (dobox ((x 0 128)
	  (y 128 129)
	  (z -128 0))
	 (sky-light-node x y z))
  (dobox ((x 0 128)
	  (y 128 129)
	  (z -128 0))
	 (light-node x y z)))

(defun avger ()
  (dobox ((x 0 128)
	  (y 0 128)
	  (z -128 0))

	 (let ((h (aref height x (1- (- z)))))
	   (if (> y h)
	       (plain-setblock x y z 0 15 0)
	       (if (and (> h y) (zerop (world:getblock x y z)))
		   (plain-setblock x y z 1 0 0)))))
  )

(defun cactglow ()
  (dobox ((x 0 128)
	  (y 0 128)
	  (z -128 0))
	 (unless (zerop (world:getblock x y z))
	   (plain-setblock x y z 1 0)))
  )

(defun trees ()
  (dobox ((x (+ 8 0) (- 128 8))
	  (z (+ 8 -128) (- 0 8)))

	 (let ((h (find-height x z)))
	   (when (zerop (random 100))
	     (when (= 2 (world:getblock x h z))
	       (tree x (1+ h) z)))))
  )

(defun cacti ()
  (dobox ((x (+ 8 0) (- 128 8))
	  (z (+ 8 -128) (- 0 8)))

	 (let ((h (find-height x z)))
	   (when (zerop (random 400))
	     (when (= 12 (world:getblock x h z))
	       (cactus x (1+ h) z)))))
  )

(defun gravel ()
  (dobox ((x (+ 8 0) (- 128 8))
	  (z (+ 8 -128) (- 0 8))
	  (y 0 20))
	 (let ((block (world:getblock x y z)))
	   (when (or (= 18 block ) (= 17 block))
	     (plain-setblock x y z 13 0 0))))
  )

(defun edge-bench ()
  (dobox ((x 0 128)
	  (y 0 128)
	  (z -128 0))
	 (let ((blockid (world:getblock x y z)))
	   (unless (zerop blockid)
	     (when (= 4 (neighbors x y z))
	       (plain-setblock x y z 58 0 0))))))

(defun corner-obsidian ()
  (dobox ((x 0 128)
	  (y 0 128)
	  (z -128 0))
	 (let ((blockid (world:getblock x y z)))
	   (unless (zerop blockid)
	     (when (= 3 (neighbors x y z))
	       (plain-setblock x y z 49 0 0))))))

(defun fill-bottom ()
  (dobox ((x 0 128)
	  (y 0 4)
	  (z -128 0))
	 (plain-setblock x y z 1 0 0)))

(defun bone?r ()
  (dobox ((x 0 128)
	  (y 0 128)
	  (z -128 0))
	 (let ((blockid (world:getblock x y z)))
	   (unless (zerop blockid)
	     (when (> 3 (neighbors x y z))
	       (plain-setblock x y z 0 0 0))))))
(defun bonder ()
  (dobox ((x 0 128)
	  (y 0 128)
	  (z -128 0))
	 (let ((blockid (world:getblock x y z)))
	   (unless (zerop blockid))
	   (when (< 4 (neighbors x y z))
	     (plain-setblock x y z 1 0 0)))))

(defun bonder ()
  (dobox ((x 0 128)
	  (y 0 128)
	  (z -128 0))
	 (let ((blockid (world:getblock x y z)))
	   (unless (zerop blockid)
	     (let ((naybs (neighbors x y z)))
	       (when (> 3 naybs)
		 
		 (plain-setblock x y z 0 0 0)))))))

(defun what? ()
  (dobox ((x 0 128)
	  (y 0 128)
	  (z -128 0))
	 (let ((blockid (world:getblock x y z)))
	   (setblock-with-update x y z blockid 0))))

(defun what? ()
  (dobox ((x 0 128)
	  (y 0 128)
	  (z -128 0))
	 (let ((blockid (world:getblock x y z)))
	   (if (= 49 blockid )
	       (setblock-with-update x y z (random 95) 0)))))

(defun dirt-sand ()
  (dobox ((x 0 128)
	  (y 0 128)
	  (z -128 0))
	 (let ((blockid (world:getblock x y z)))
	   (case blockid
	     (2 (setblock-with-update x y z 12 0))
	     (3 (setblock-with-update x y z 24 0))))))

(defun clearblock? (id)
  (declare (type fixnum id))
  (dobox ((x 0 128)
	  (y 0 128)
	  (z -128 0))
	 (let ((blockid (world:getblock x y z)))
	   (when (= blockid id)
	     (plain-setblock x y z 0 0)))))

(defun clearblock? (id)
  (declare (type fixnum id))
  (dobox ((x 0 128)
	  (y 0 128)
	  (z -128 0))
	 (let ((blockid (world:getblock x y z)))
	   (when (= blockid id)
	     (plain-setblock x y z 0 0)))))

(defun clcok? ()
  (dobox ((x 0 128)
	  (y 0 128)
	  (z -128 0))
	 (let ((blockid (world:skygetlight x y z)))
	   (when 
	     (< blockid 12)
	     (plain-setblock x y z 1 0)))))

(defun grassify ()
  (dobox ((x 0 128)
	  (y 0 128)
	  (z -128 0))
	 (let ((blockid (world:getblock x y z)))
	   (when (= blockid 3)
	     (let ((idabove (world:getblock x (1+ y) z)))
	       (when (zerop idabove)
		 (plain-setblock x y z 2 0)))))))

(defun dirts ()
  (dobox ((x 0 128)
	  (y 0 128)
	  (z -128 0))
	 (let ((blockid (world:getblock x y z)))
	   (when (= blockid 1)
	     (when (or (zerop (world:getblock x (+ 2 y) z))
		       (zerop (world:getblock x (+ 3 y) z)))
	       (plain-setblock x y z 3 0))))))

(defun tree (x y z)
  (let ((trunk-height (+ 1 (random 3))))
    
    (dobox ((z0 -2 3)
	    (x0 -2 3))
	   (plain-setblock (+ x x0) (+ y trunk-height) (+ z z0) 18 0 0))
    (dobox ((z0 -2 3)
	    (x0 -2 3))
	   (plain-setblock (+ x x0) (+ y trunk-height 1) (+ z z0) 18 0 0))
    (flet ((edge (a b)
	     (unless (zerop (random 4))
	       (plain-setblock (+ a x) (+ y trunk-height 1) (+ b z) 0 0 0))))
      (edge 2 2)
      (edge 2 -2)
      (edge -2 2)
      (edge -2 -2))
    (flet ((edge (a b)
	     (unless (zerop (random 4))
	       (plain-setblock (+ a x) (+ y trunk-height) (+ b z) 0 0 0))))
      (edge 2 2)
      (edge 2 -2)
      (edge -2 2)
      (edge -2 -2))
    (dobox ((x0 -1 2)
	    (z0 -1 2))
	   (plain-setblock (+ x x0) (+ y trunk-height 2) (+ z z0) 18 0 0))
    (flet ((edge (a b)
	     (plain-setblock (+ a x) (+ y trunk-height 3) (+ b z) 18 0 0)))
      (edge 0 1)
      (edge 1 0)
      (edge 0 -1)
      (edge -1 0)
      (edge 0 0))
    (dobox ((y0 y (+ y (+ 3 trunk-height))))
	   (plain-setblock x y0 z 17 0 0))))

(defun cactus (x y z)
  (let ((trunk-height (+ 3 (random 1))))
    (dobox ((y0 0 trunk-height))
	   (plain-setblock (+ x 0) (+ y y0) (+ z 0) 81 0 0))))

(defun meh ()
  (goto 64 128 -64))

(defparameter grass-strands
  (vector 3 2 3 3
	  4 1 3 2
	  4 3 4 3
	  2 3 3 2))

(defun grasscolor ()
  (let ((img (get-image "terrain.png"))
	(tot (vector 0 0 0 0))
	(amount 0))
    (dotimes (x 16)
      (let ((xpos (+ 48 x)))
	(dotimes (yoff (aref grass-strands x))
	  (incf amount)
	  (let ((ypos (- 255 yoff)))
	    (let ((pixel (imagewise:getapixel ypos xpos img)))
	      (map-into tot #'+ pixel tot))))))
    (print (list tot amount))))

(defun emptycolor ()
  (let ((img (get-image "terrain.png"))
	(tot (vector 0 0 0 0)))
    (dotimes (x 16)
      (let ((xpos x))
	(dotimes (yoff 16)
	  (let ((ypos (- 255 yoff)))
	    (let ((pixel (imagewise:getapixel ypos xpos img)))
	      (map-into tot #'+ pixel tot))))))
    (print tot)))

(defun stirling (n)
  (* (truncate (sqrt (* 2 pi n))) (expt (truncate (/ n (exp 1))) n)))

(defun fucke (n)
  (+ (* n (log n)) (- n) (* 1/2 (log (* 2 pi n)))))

(defun hashpop (pop size)
  (let ((large (fucke size))
	(small (fucke (- size pop))))
    (let ((val (- large small))
	  (other (* pop (log size))))
      (let ((exponent (- val other)))
	(print exponent)
	(print (* 100 (- 1 (exp exponent))))))))

 (defun file-string (path)
   (with-open-file (stream path)
     (let ((data (make-string (file-length stream))))
       (read-sequence data stream)
       data)))

(progn
 (defpackage #:pathwise
   (:use #:cl)
   (:export

    #:file-string
    #:byte-read
    #:expand-paths))
 (in-package :pathwise)
;;;load a file into a string
;;;load a file into a byte-array
 (defun byte-read (path)
   (with-open-file (stream path :element-type '(unsigned-byte 8))
     (let* ((len (file-length stream))
	    (data (make-array len :element-type '(unsigned-byte 8))))
       (dotimes (n len)
	 (setf (aref data n) (read-byte stream)))
       data)))
;;;takes a tree list structure where the car of each list is the folder name ex: #P"root/"
;;;and the other items in the rest of the list are either more lists or just plain strings.
;;;returns a flat list which is the collection of all the resulting pathnames

 (defun expand-paths (dir-list)
   (let (acc) 
     (labels ((rec (x currentpath)
		(if (consp x)
		    (dolist (sub (cdr x))
		      (rec sub (merge-pathnames (car x) currentpath)))
		    (push (merge-pathnames x currentpath) acc))))
       (rec dir-list #P"") acc))))

(in-package :sandbox)

(defun setatest (x)
  (prog2 (setf atest
	       (case x
		 (0 (byte-read #P "/home/terminal256/.minecraft/saves/New World-/region/r.0.-1.mcr"))
		 (1 (byte-read #P "/home/imac/.minecraft/saves/New World/region/r.0.1.mcr"))
		 (2 cl-mc-shit::testchunk)
		 ))
      x))

(defparameter atest nil)
;(setatest 2)

(defun someseq (x y)
  (let* ((thechunk (helpchunk x y)))
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

(defun nbt-open (lizz)
  (third
   (first
    (third
     lizz))))

(defun gettag (lestring lizz)
  (dolist (tag lizz)
    (if (equal lestring (second tag))
	(return-from gettag (third tag)))))

(defun getmetadatalizz (lizz)
  (expand-nibbles
   (gettag "Data"
	   (nbt-open lizz))))

(defun getskylightlizz (lizz)
  (expand-nibbles
   (gettag "SkyLight"
	   (nbt-open lizz))))

(defun getlightlizz (lizz)
  (expand-nibbles
   (gettag "BlockLight"
	   (nbt-open lizz))) )

(defun getblockslizz (lizz)
  (gettag
   "Blocks"
   (nbt-open lizz)))

(defun getheightlizz (lizz)
  (gettag
   "HeightMap"
   (nbt-open lizz)))


(defun draw-sky ()
  (progn
    (set-matrix "projectionmodelview"
		(cg-matrix:transpose-matrix
		 *projection-view-matrix*))
    (bind-shit :skybox)
    (gl:bind-texture :texture-2d *framebuffer-texture*)
    (ldrawlist :skybox))
  (progn
    (let ((time (daytime)))
      (set-matrix "projectionmodelview"
		  (cg-matrix:%transpose-matrix
		   *temp-matrix*
		   (cg-matrix:matrix*
		    *projection-view-matrix*
		    (cg-matrix:rotate-around
		     (cg-matrix:vec -1.0 0.0 0.0)
		     time)
		    (cg-matrix:scale* 10.0 10.0 90.0))))

      (gl:enable :blend)
      (gl:blend-func :src-alpha :src-alpha)
      (bind-shit :sun)
      (ldrawlist :sun)
      
      (bind-shit :moon)
     (ldrawlist :moon))
    (gl:disable :blend)))

(defun daytime ()
  (coerce (* (get-internal-run-time)
	     (/ 840.0 100000000.0))
	  'single-float))

(defun delete-shaders ()
  (clrhash *g/text*))


