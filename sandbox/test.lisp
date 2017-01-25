(in-package :sandbox)

;;;used by none but to test

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
  (dobox ((x 0 8) (y -8 0))
	 (someseq x y)))

(defun spawn ()
  (goto 64 80 -64))

(defun goto (x y z)
  (setf *xpos* x
	*ypos* y
	*zpos* z))

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
					  (get-image"misc/grasscolor.png"))))
  (let ((terrain (get-image "terrain.png")))
    (dobox ((x xpos (+ 16 xpos)) (y ypos (+ 16 ypos)))
	   (multiply-into (imagewise:getapixel y x terrain) color))))


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
  (let ((text (pathwise:file-string path)))
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

(defun all-zeroes-p (sequence)
  (dotimes (x (length sequence))
    (unless (zerop (aref sequence x))
      (return-from all-zeroes-p nil)))
  t)

(defun test= (a b)
  (declare (type symbol a b)
	   (optimize (speed 3) (safety 0)))
  (eq a b))

(defparameter *save* #P"second/")

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
	  (y 64 128)
	  (z -128 0))
	 (let ((blockid (world:getblock x y z)))
	   ;(unless (zerop blockid))
	   (plain-setblock x y z blockid 0 0)))
  (dobox ((x 0 128)
	  (y 128 129)
	  (z -128 0))
	 (sky-light-node x y z)))

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

(defun stone? ()
  (dobox ((x 0 128)
	  (y 0 128)
	  (z -128 0))
	 (let ((blockid (world:getblock x y z)))
	   (when (= 1 blockid)
	     (when (> 4 (neighbors x y z))
	       (plain-setblock x y z 0 0 0))))))

(defun what? ()
  (dobox ((x 0 128)
	  (y 0 128)
	  (z -128 0))
	 (let ((blockid (world:getblock x y z)))
	   (setblock-with-update x y z blockid 0))))

(defun meh ()
  (goto 64 128 -64))
