(in-package :sandbox)

;;;used by none but to test

(defun spill-hash (hash)
  (loop for key being the hash-keys of hash
     using (hash-value value)
     do (format t "~S ~S~%" key value)))

(defun int-scale (int scale)
  (truncate (* int scale)))

(defun fun-setup ()
  (color-grasses)
  (test-world)
  (erase-bottom)
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

