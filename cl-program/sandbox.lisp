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
  (macrolet ((k (val)
	       (nthcdr 3 val)))
    (k (hook:add-hook init-hook :call-list (lambda () (clrhash *g/call-list*))))
    (k (hook:add-hook init-hook :texture (lambda () (clrhash *g/texture*))))
    (k (hook:add-hook init-hook :shader (lambda () (clrhash *g/shader*))))
    (k (hook:add-hook init-hook :chunk-call-list (lambda () (clrhash *g/chunk-call-list*)))))
  ;;(hook:run-hook init-hook)
  (glinnit) ;opengl
  (physinnit) ;physics
  (injection)) 

(defun injection ()
  (window:poll)
  (physics)
  (set-render-cam-pos *camera*)
  (remove-spurious-mouse-input)
  (render)
  (incf *ticks*)
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

(progno
 (color-grasses)
 (goto 16 80 -16))

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

(defun all-zeroes-p (sequence)
  (dotimes (x (length sequence))
    (unless (zerop (aref sequence x))
      (return-from all-zeroes-p nil)))
  t)

(defun test= (a b)
  (declare (type symbol a b)
	   (optimize (speed 3) (safety 0)))
  (eq a b))

(defparameter *save* #P"third/")

(defparameter *saves-dir* (merge-pathnames #P"saves/" ourdir))

(defun save (filename &rest things)
  (let ((path (merge-pathnames filename *saves-dir*)))
    (with-open-file (stream path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (dolist (thing things)
	(print thing stream)))))

(defun save2 (thingfilename &rest things)
  (apply #'save (merge-pathnames (format nil "~s" thingfilename) *save*) things))

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


