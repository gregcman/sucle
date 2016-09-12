(in-package #:sandbox)

(defmacro progno (&rest nope))

(defun loud-thread (func name)
  (sb-thread:make-thread ;; thread function
   #'(lambda (standard-output standard-input)
       ;; thread-local dynamic binding of special variable
       (let ((*standard-output* standard-output) (*standard-input* standard-input))
	 (funcall func)))
   ;; thread function argument, provided by the current thread
   :arguments (list *standard-output* *standard-input*)
   :name name))

(defun main
    (&rest args)
  (window:arise)
  (setq dathread nil)
  (if (not args)
      (setq dathread (loud-thread (lambda () (funcall window:wrapper #'init)) "ourthread")) 
      (funcall window:wrapper #'init)))

(defparameter dathread nil)
(defparameter resdir
  #P"/home/imac/quicklisp/local-projects/cocktus/res/")
(defparameter shaderdir #P "shaders/")

(defparameter texture-library (make-hash-table :test 'equal))
(defparameter picture-library (make-hash-table :test 'equal))
(defparameter shader-library (make-hash-table :test 'equal))

(defparameter physthread nil)
(defparameter kill-button t)
(defun init ()
  (setq kill-button t)
  (setq out:width (if nil 512 854) out:height (if nil 512 480))
  (out:push-dimensions)
  (in::initialize)
  (glinnit)
  (in:p-1 #\q (let ((variabl nil))
		(lambda ()
		  (setf variabl (not variabl))
		  (if variabl
		      (gl:polygon-mode :front-and-back :line)
		      (gl:polygon-mode :front-and-back :fill)))))
  (in:p+1 #\ (lambda () (setq kill-button nil)))
  (in:p+1 #\v #'leresize)
  (in:p+1 #\r (function window:toggle-mouse-capture))
  (in:p+1 5 (lambda () (incf fov)))
  (in:p+1 4 (lambda () (decf fov)))
  (in:p+1 #\3 (lambda () (print (/ 1000.0 physrate))))
  (in:p+1 #\2 (lambda () (print (/ 1000.0 renderrate))))
  (in:p+1 #\5 (lambda () (print cameraPos)))
  (in:p+1 3 (lambda () (aplatform
		       (round (row-major-aref cameraPos 0))
		       (round (row-major-aref cameraPos 1))
		       (round (row-major-aref cameraPos 2))
		       (random 97))))
  (setq camerapos (mat:onebyfour '(8 70 8 1)))
  (setq cameraVelocity (mat:onebyfour '(0 0 0 0)))
  (load-shit
   (fatten
    (gethash "terrain.png" picture-library))
   
   "terrain.png" 256 256)
  (bind-shit "terrain.png")

  (setf dirtychunks nil)
  (maphash
   (lambda (key value)
     (declare (ignore value))
     (push key dirtychunks))
   chunkhash)

  (setf vaohash (make-hash-table :test #'equal))
  (setf phystimer (timer))
  (setf rendertimer (timer))
  (setf physthread (loud-thread #'physthread "physics"))
  (unwind-protect
       (injection)
    (sb-thread:terminate-thread physthread)))

(defun leresize ()
  (out:push-dimensions)
  (gl:viewport 0 0 out:width out:height))

(defun get-all-mc-textures ()
  (let* ((shit 
	  (flatten
	   (get-file-paths-and-metadata png-resources resdir))))
    (labels ((rec (lizzy)
	       (if lizzy
		   (let ((nombre (first lizzy))
			 (path (second lizzy)))
		     (load-png nombre path)
		     (rec (cddr lizzy))))))
      (rec shit))))

(defparameter phystimer nil)
(defparameter physrate nil)
(defun physthread ()
  (let ((arate (funcall phystimer
		 (/ 1000.0 60)
		 (lambda ()
		   (if (window:ismousecaptured)
		       (controls))
		   (physics)))))
    (if arate
	(setf physrate arate)))
  (if (and
       kill-button
       (not window:status))
      (physthread)))

(defun timer ()
  (let ((prevtime (get-internal-real-time)))
    (lambda (time afunc)
      (let* ((now (get-internal-real-time))
	     (diff (- now prevtime)))
	(if (> diff time)
	    (progn
	      (setf prevtime now)
	      (funcall afunc)
	      diff)
	    nil)))))


(defparameter rendertimer nil)
(defparameter renderrate nil)

(defparameter ticks/sec 60)

(defparameter tickscale (/ 20 ticks/sec))

(defun injection ()
  (let ((arate (funcall rendertimer
		 (/ 1000.0 ticks/sec)
		 (lambda ()
		   (funcall window:base-needs)
		   (draw)))))
    (if arate
	(setf renderrate arate)))
  (if 
   (and
    kill-button
    (not window:status))
   (injection)))

(defparameter onground nil)

(defun physics ()
  (let* ((newpos (mat:add cameraPos cameraVelocity))
	 (blockid (mat-pos newpos)))
    (mat:add! cameraPos cameraVelocity)
    (if (not (= 0 blockid))
	(progn
	  (setf (row-major-aref cameraPos 1)
		(- (ceiling (row-major-aref cameraPos 1)) 0.5))
	  (setf (row-major-aref cameraVelocity 1)
		0)
	  (setf onground t))
	(setf onground nil))
    (if (> 0 (row-major-aref cameraPos 1))
	(progn
	  
	  (setf (row-major-aref cameraVelocity 1)
		0)
	  (setf (row-major-aref cameraPos 1)
		0)
	  (setq camerapos (mat:onebyfour '(0 128 0 1))))))
  
  (mat:add! cameraVelocity (mat:onebyfour (list 0 (* -0.08 (expt tickscale 2)) 0 0)))
  (let ((airscaled (mat:onebyfour (list
				   (row-major-aref cameraVelocity 0)
				   0
				   (row-major-aref cameraVelocity 2)
				   0))))
    (mat:scale! airscaled (* 0.6 0.91 0.5))
    (setf (row-major-aref cameraVelocity 0) (row-major-aref airscaled 0))
    (setf (row-major-aref cameraVelocity 2) (row-major-aref airscaled 2)))

  (setf (row-major-aref cameraVelocity 1) (* (expt 0.98 tickscale) (row-major-aref cameraVelocity 1))))

(defun controls ()
  (mouse-looking)
  (keymovement))

(defun mouse-looking ()
  (let* ((change (in:delta))
	 (x (* 1/360 (aref change 0)))
	 (y (* 1/360 (aref change 1))))
    (setq yaw (mod (+ yaw x) (* 2 pi)))
    (setq pitch (anothershit (+ pitch y) (/ pi 2)))))

(defun anothershit (x whatthefuck)
  (if (> x whatthefuck)
      whatthefuck
      (if (< x (- whatthefuck))
	  (- whatthefuck)
	  x)))

(defun good-func (some-number)
  (lambda (x)
	    (if (in:key-p (first x))
		(mat:add! some-number
			  (mat:onebyfour (second x))))))

(defun empty-vec4 ()
  (mat:onebyfour '(0 0 0 0)))

(defun key-legs ()
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
  (let* ((delta (empty-vec4))
	 (lemod (good-func delta)))
    (if onground
	
	(mapcar
	 lemod
	 `((#\Space (0 ,(* 0.42 (expt tickscale 1)) 0 0)))))
    delta))

(defun keymovement () 
  (mat:add! cameraVelocity
	    (mat:mmul! (mat:add (key-legs) (key-jumps))
		       (mat:rotation-matrix 0 1 0 yaw))))


(defun mat-pos (mat)
  (getblock
   (round (row-major-aref mat 0))
   (round (row-major-aref mat 1))
   (round (row-major-aref mat 2))))

(defparameter vaohash nil)
(defun render ()
  (set-matrix "view" (mat:easy-lookat (mat:add #2A((0 1.5 0 0)) camerapos) pitch yaw))
  (set-matrix "projection"  (mat:projection-matrix
			     (deg-rad fov)
			     (/ out::pushed-width out::pushed-height) 0.01 100)) 
  (set-matrix "model" (mat:identity-matrix))
  (let ((achunk (pop dirtychunks)))
    (if achunk
	(update-chunk-vao achunk)))
  (if (in:key-pressed-p #\g)
      (update-world-vao))
  (maphash
   (lambda (key vao)
     (set-matrix
      "model"
      (mat:translation-matrix
       (* 16 (first key))(* 16 (second key)) (* 16 (third key))))
     (draw-vao vao))
   vaohash))

(defun update-world-vao ()
  (maphash
   #'update-chunk-vao
   chunkhash))

(defun update-chunk-vao (key &optional (chunk (gethash key chunkhash)))
  (let ((old (gethash key vaohash)))
    (progno (if old
		(destroy-vao old)))
    (setf (gethash key vaohash)
	  (shape-vao (chunk-shape chunk key)))))

(defun empty-chunk ()
  (make-array '(16 16 16)))

(defparameter chunkhash (make-hash-table :test (function equal)))
(defparameter dirtychunks nil)

(defun asamplechunk ()
  (dotimes (x 8)
    (dotimes (y 8)
      (dotimes (p (length achunk))
	(setf (gethash (list (- x 4) p (- y 4)) chunkhash)
	      (flat-chunk (elt achunk p) 16 16 16))))))

(defun flat-chunk (data x y z)
  (let ((new-chunk (make-array (list x y z))))
    (dotimes (i x)
      (dotimes (j y)
	(dotimes (k z)
	  (setf (row-major-aref new-chunk (+ (* i) (* j x) (* k x y)))
		(aref data (+ (* i 16) (* j 16 16) (* k)))))))
    new-chunk))

(defmacro chunk-block (chunk i j k)
  `(aref ,chunk ,i ,j ,k))
(defun getblock (i j k)
  (multiple-value-bind (x xd) (floor i 16)
    (Multiple-value-bind (y yd) (floor j 16)
      (multiple-value-bind (z zd) (floor k 16)
	(let ((chunk (gethash (list x y z) chunkhash)))
	  (if chunk
	      (chunk-block chunk xd yd zd)
	      0))))))

(defun empty-chunk-at (key)
  (let ((newchunk (empty-chunk)))
    (setf
     (gethash key chunkhash)
     newchunk)
    newchunk))

(defun setblock (i j k blockid)
  (multiple-value-bind (x xd) (floor i 16)
    (multiple-value-bind (y yd) (floor j 16)
      (multiple-value-bind (z zd) (floor k 16)
	(let* ((pos (list x y z))
	       (chunk (gethash pos chunkhash)))
	  (if (not chunk)
	      (setf chunk (empty-chunk-at pos)))
	  (let ((old (chunk-block chunk xd yd zd)))
	    (if (/= old blockid)
		(setf (chunk-block chunk xd yd zd) blockid)
		nil)))))))

(defun setblock-with-update (i j k blockid)
  (if (setblock i j k blockid)
      (progn
	 (pushnew (list
		   (floor i 16)
		   (floor j 16)
		   (floor k 16))
		  dirtychunks :test 'equal))))

(defun seed (times val rad)
  (dotimes (n times)
    (setblock
     (+ 0 (random rad))
     (+ 64 (random rad))
     (+ 0 (random rad))
     val)))

(defun aplatform (i j k blockid)
  (dotimes (a 3)
    (dotimes (b 3)
      (setblock-with-update (+ a i -1) (1- j) (+ b k -1) blockid))))

(defun yay (x)
  (- (random (+ x x)) x))

(in-package :sandbox)

(eval-when (:load-toplevel :execute)
  (defparameter png-resources
    '("moreshit/"
      "terrain.png"
      "pack.png" "particles.png" 
      ("achievement/"
       ("bg.png" "icons.png"))
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
       "black.png" "mclogo.png" "mojang.png"))))

(eval-when (:load-toplevel :execute)
  (get-all-mc-textures))

;;
;;
;;

;;
;;
;;

(defparameter i-face
  '((-0.5  0.5  0.5  1.0 1.0  0.6 0.6 0.6 0.6)
    (-0.5  0.5 -0.5  0.0 1.0  0.6 0.6 0.6 0.6 )
    (-0.5 -0.5 -0.5  0.0 0.0  0.6 0.6 0.6 0.6 )
    (-0.5 -0.5  0.5  1.0 0.0  0.6 0.6 0.6 0.6 )))
(defparameter i+face
  '((0.5 -0.5  0.5  1.0 0.0  0.6 0.6 0.6 0.6)
    (0.5 -0.5 -0.5  0.0 0.0  0.6 0.6 0.6 0.6)
    (0.5  0.5 -0.5  0.0 1.0  0.6 0.6 0.6 0.6)
    (0.5  0.5  0.5  1.0 1.0  0.6 0.6 0.6 0.6 )))
(defparameter j-face
  '((-0.5 -0.5 -0.5  0.0 0.0  0.3 0.3 0.3 0.3)
    (0.5 -0.5 -0.5  1.0 0.0  0.3 0.3 0.3 0.3)
    (0.5 -0.5  0.5  1.0 1.0  0.3 0.3 0.3 0.3)
    (-0.5 -0.5  0.5  0.0 1.0  0.3 0.3 0.3 0.3)))
(defparameter j+face
  '((-0.5 0.5 0.5 0.0 1.0  1.0 1.0 1.0 1.0)
    (0.5 0.5 0.5 1.0 1.0  1.0 1.0 1.0 1.0)
    (0.5 0.5 -0.5 1.0 0.0  1.0 1.0 1.0 1.0)
    (-0.5 0.5 -0.5 0.0 0.0  1.0 1.0 1.0 1.0)))
(defparameter k-face
  '((-0.5 0.5 -0.5 0.0 1.0  0.8 0.8 0.8 0.8)
    (0.5 0.5 -0.5 1.0 1.0  0.8 0.8 0.8 0.8)
    (0.5 -0.5 -0.5 1.0 0.0  0.8 0.8 0.8 0.8)
    (-0.5 -0.5 -0.5 0.0 0.0  0.8 0.8 0.8 0.8)))
(defparameter k+face
  '((-0.5 -0.5  0.5  0.0 0.0 0.8 0.8 0.8 0.8 )
    (0.5 -0.5  0.5  1.0 0.0  0.8 0.8 0.8 0.8 )
    (0.5  0.5  0.5  1.0 1.0  0.8 0.8 0.8 0.8)
    (-0.5  0.5  0.5  0.0 1.0 0.8 0.8 0.8 0.8)))

(defparameter cube-shape
  (reduce #'add-verts
	  (list i-face i+face j-face j+face k-face k+face)
	  :initial-value (make-shape)))

(defun translate-verts (translation-matrix verts)
  (mapcar
   (lambda (v)
     (let ((x (first v))
	   (y (second v))
	   (z (third v)))
       (let ((new-vert
	      (mat:fourbyone
	       (list x y z 1))))
	 (let ((trans (mat:mmul translation-matrix new-vert)))
	   (cons
	    (row-major-aref trans 0)
	    (cons
	     (row-major-aref trans 1)
	     (cons
	      (row-major-aref trans 2)
	      (nthcdr 3 v))))))))
   verts))

(defun translate-shape (s translation-matrix)
  (let ((verts (shape-vs s)))  
    (setf (shape-vs s)
	  (translate-verts verts translation-matrix))
    s))

(defun increment-verts (x y z verts)
  (dolist (n verts)
    (incf (first n) x)
    (incf (second n) y)
    (incf (third n) z))
  verts)

(defun %damn-fuck (verts num)
  (let* ((xtrans (mod num 16))
	 (ytrans (- 15 (/ (- num xtrans) 16))))
    (dolist (v verts)
      (setf (fourth v) (+ (/ (fourth v) 16) (/ xtrans 16)))
      (setf (fifth v) (+ (/ (fifth v) 16) (/  ytrans 16))))
    verts))

(defun something... (a b c d e f)
  (getblock (+ a (* 16 d))(+ b (* 16 e))(+ c (* 16 f))))

(defun chunk-shape (chunk coords)
  (let* ((new-shape (make-shape))
	 (dims (array-dimensions chunk))
	 (il (first dims))
	 (jl (second dims))
	 (kl (third dims))
	 (io (first coords))
	 (jo (second coords))
	 (ko (third coords)))
    (flet ((getblock (x y z)
	     (chunk-block chunk x y z))
	   (getempty (x y z)
	     (if (and
		  (< -1 x il)
		  (< -1 y jl)
		  (< -1 z kl))
		 (if
		  (/= 0 (chunk-block chunk x y z))
		  nil
		  (= 0 (something... x y z io jo ko)))
		 (= 0 (something... x y z io jo ko)))))
      (dotimes (i il)
	(dotimes (j jl)
	  (dotimes (k kl)
	    (let ((blockid (getblock i j k)))
	      (if (not (zerop blockid))
		  (let ((the-skin (aref blockIndexInTexture blockid)))
		    (if (numberp the-skin)
			(reduce
			 #'add-verts
			 (get-a-default-block-shape #'getempty the-skin i j k)
			 :initial-value new-shape)
			(reduce
			 #'add-verts
			 (get-a-variable-block-shape #'getempty i j k the-skin)
			 :initial-value new-shape)))))))))
    new-shape))

(defun get-a-variable-block-shape (getempty i j k func)
  (let* ((lefaces nil))
    (if
     (funcall getempty (1+ i) j k)
     (push 
      (%damn-fuck
       (copy-tree
	i+face)
       (funcall func 3))
      lefaces))
    (if
     (funcall getempty (1- i) j k)
     (push
      (%damn-fuck
       (copy-tree i-face)
       (funcall func 2))
      lefaces))
    (if
     (funcall getempty i (1+ j) k)
     (push
      (%damn-fuck
       (copy-tree j+face)
       (funcall func 1))
      lefaces))
    (if
     (funcall getempty i (1- j) k)
     (push
      (%damn-fuck
       (copy-tree j-face)
       (funcall func 5))
      lefaces))
    (if
     (funcall getempty i j (1+ k))
     (push
      (%damn-fuck
       (copy-tree k+face)
       (funcall func 4))
      lefaces))
    (if
     (funcall getempty i j (1- k))
     (push
      (%damn-fuck
       (copy-tree k-face)
       (funcall func 2))
      lefaces))
    (dolist (f lefaces)
      (increment-verts i j k f))
    lefaces))

(defun get-a-default-block-shape (getempty skin i j k)
  (let* ((faces nil))
    (if
     (funcall getempty (1+ i) j k)
     (push (copy-tree i+face) faces))
    (if
     (funcall getempty (1- i) j k)
     (push (copy-tree i-face) faces))
    (if
     (funcall getempty i (1+ j) k)
     (push (copy-tree j+face) faces))
    (if
     (funcall getempty i (1- j) k)
     (push (copy-tree j-face) faces))
    (if
     (funcall getempty i j (1+ k))
     (push (copy-tree k+face) faces))
    (if
     (funcall getempty i j (1- k))
     (push (copy-tree k-face) faces))
    (dolist (face faces)
      (%damn-fuck face skin)
      (increment-verts i j k face))
    faces))

(defparameter achunk
  (list
   (vector
    7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 1 7 1 7 1 7 7 7 7 7 7 7 7 7 1 7 1 7 7 1 1 7 7 1 7 7 7 7 7 7 1 7 7 7 7 7 7 7 1 1 7 1 16 7 7 7 7 7 1 7 7 1 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 1 7 1 7 7 7 7 7 7 7 7 1 7 1 7 7 7 7 7 1 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 1 1 7 7 7 1 7 7 7 1 7 7 7 7 7 7 1 7 1 7 7 7 7 7 7 7 1 7 7 7 7 1 1 7 7 7 1 7 7 7 1 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 1 7 1 7 7 7 1 1 1 7 1 13 13 7 7 7 7 1 1 7 7 7 1 7 7 1 7 7 7 7 13 1 7 7 1 7 7 7 7 1 7 13 7 13 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 13 7 7 7 1 1 7 7 7 7 1 7 7 7 1 7 1 1 1 1 1 7 1 7 1 1 7 1 7 7 1 1 7 7 7 1 7 7 7 7 7 7 7 7 7 1 1 1 7 7 1 7 7 7 7 7 7 1 1 7 1 7 7 7 7 7 7 1 7 16 16 16 1 7 7 1 7 7 7 7 1 1 7 7 7 7 7 7 1 7 1 7 1 7 15 7 1 7 7 1 1 7 1 7 7 7 1 7 7 1 1 7 7 7 1 7 7 1 1 7 7 7 1 1 1 1 1 7 7 7 7 7 1 1 1 1 1 7 7 7 7 7 1 1 7 7 1 1 1 7 7 7 7 7 7 1 7 1 1 1 1 7 7 1 7 7 7 1 1 1 7 7 7 1 7 7 7 7 7 7 7 7 7 7 1 1 1 1 1 1 1 7 1 7 7 7 7 1 7 1 1 7 1 7 7 7 7 7 1 7 7 1 7 7 7 7 1 7 1 7 1 13 13 13 7 7 7 7 1 7 1 7 7 1 1 7 13 7 13 13 13 7 1 1 1 1 1 1 1 7 1 13 7 7 7 7 7 7 7 7 7 7 1 7 7 1 7 7 7 7 7 1 1 1 7 1 1 7 1 7 7 1 1 7 1 7 1 1 7 1 7 7 7 7 1 7 1 7 1 1 7 1 14 1 1 1 1 7 1 1 1 1 1 7 1 1 7 7 1 1 1 1 7 1 7 1 1 1 7 7 7 1 1 1 1 1 1 7 1 7 1 1 7 1 7 1 1 1 1 1 1 7 1 1 1 1 7 1 1 1 1 1 7 1 1 1 7 1 1 7 1 1 7 1 7 1 1 7 1 1 7 1 1 1 1 1 1 1 1 1 1 7 1 7 1 7 1 7 1 7 1 7 7 7 1 1 7 1 1 1 1 1 1 1 7 7 7 1 1 7 7 1 1 7 7 1 7 1 1 1 1 1 1 1 7 7 1 7 1 7 1 7 7 1 1 1 7 7 1 1 1 7 7 1 7 1 1 1 1 1 1 7 7 7 1 1 7 1 1 1 7 7 1 1 1 1 7 1 13 1 1 1 1 7 1 1 1 1 1 7 1 7 1 1 13 13 7 7 1 1 1 1 7 1 1 1 1 1 7 7 1 1 7 1 1 7 1 1 1 1 7 1 7 1 1 7 1 1 1 7 1 1 7 1 1 1 1 1 1 1 1 1 1 7 1 7 1 1 1 1 1 1 1 1 1 1 14 14 14 1 1 1 7 1 1 1 7 1 1 7 7 1 14 14 7 1 1 7 1 1 1 7 1 1 7 7 1 1 1 7 1 7 1 1 1 7 1 7 1 7 7 1 7 1 7 1 1 7 1 1 1 1 1 1 1 7 1 1 1 1 1 1 1 7 1 1 7 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 7 1 7 1 7 1 1 1 1 1 1 1 1 7 1 1 1 1 1 1 1 7 1 1 7 1 1 7 1 1 1 1 1 1 7 1 7 1 7 1 7 7 1 73 1 1 1 14 7 1 1 1 1 1 1 1 1 1 1 1 1 1 14 1 1 1 1 7 1 1 1 1 1 1 1 1 1 1 7 1 1 1 1 1 7 7 1 1 1 1 1 7 7 7 7 1 1 7 1 1 1 7 1 1 1 1 1 1 7 1 1 1 1 1 1 7 1 1 1 1 7 1 1 1 7 7 1 1 1 1 1 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 14 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 73 73 1 1 1 1 1 1 1 1 1 1 1 1 1 1 73 73 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 73 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 73 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 73 73 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 73 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 73 73 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 56 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 21 1 13 13 13 1 1 1 1 1 16 16 16 1 1 1 1 1 13 13 13 13 1 1 1 1 1 16 16 1 1 1 1 1 1 13 13 13 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 56 56 1 1 1 1 1 1 1 1 1 1 1 1 1 1 56 56 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 21 21 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 13 13 13 13 13 1 1 1 1 16 16 16 1 73 1 1 13 13 13 13 13 13 1 1 1 1 16 16 1 1 1 1 1 13 13 13 13 13 13 13 1 1 1 1 1 1 1 1 1 1 13 13 13 13 13 13 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 13 13 13 13 13 13 1 1 1 3 3 3 3 3 3 1 13 13 13 13 13 13 13 1 1 3 3 3 3 1 1 1 1 13 13 13 13 13 13 13 1 1 1 1 1 1 1 1 1 1 13 13 13 13 13 13 13 1 1 1 1 1 1 1 1 1 1 1 1 13 13 13 13 73 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 16 1 1 1 1 1 1 1 1 1 1 0 0 0 1 16 16 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 16 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 16 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 13 13 13 13 13 13 1 1 3 3 3 3 3 3 3 3 1 13 13 13 13 13 1 3 3 3 3 3 3 3 3 1 1 13 13 13 13 13 13 13 3 3 3 3 3 1 1 1 1 1 13 13 13 13 13 13 13 1 73 1 1 1 1 1 1 1 1 1 1 13 13 13 13 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 16 1 1 1 1 1 1 1 1 1 1 0 0 0 0 16 16 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 16 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 16 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 13 1 1 1 3 3 3 3 3 3 3 3 3 1 1 13 13 13 1 3 3 3 3 3 3 3 3 3 1 1 1 13 13 13 1 13 3 3 3 3 3 3 1 1 1 1 1 1 1 1 1 13 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 73 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3 3 13 1 1 1 1 1 1 3 3 3 3 3 3 3 3 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 3 3 1 1 1 1 0 0 0 1 1 1 1 1 1 3 3 3 3 1 1 1 0 0 0 1 1 1 1 1 1 3 3 3 3 1 1 1 0 0 0 1 1 1 1 1 1 3 3 3 3 1 1 1 0 0 0 0 1 1 1 1 1 3 3 3 3 1 1 1 0 0 0 0 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 13 13 13 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 3 1 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 13 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 13 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 13 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 13 1 1 1 1 1 1 1 1 3 3 1 1 1 1 0 0 0 1 1 1 1 1 1 3 3 3 1 1 1 1 0 0 0 1 1 1 1 1 1 3 3 3 3 1 1 1 0 0 0 0 1 1 1 1 1 3 3 3 3 1 1 1 0 0 0 0 1 1 1 1 1 3 3 3 3 1 1 1 0 0 0 0 1 1 1 1 1 3 3 3 3 1 1 1 0 0 0 0 0 1 1 1 1 3 3 3 3 1 1 1 0 0 0 0 0 1 1 1 1 3 3 3 3 1 1 1)

   (vector  1 1 1 1 1 1 1 1 1 1 1 1 1 1 13 13 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 13 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 13 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 13 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 13 1 1 1 1 1 1 1 1 3 3 1 1 1 1 0 0 0 1 1 1 1 1 1 3 3 3 1 1 1 1 0 0 0 1 1 1 1 1 1 3 3 3 3 1 1 1 0 0 0 0 1 1 1 1 1 3 3 3 3 1 1 1 0 0 0 0 1 1 1 1 1 3 3 3 3 1 1 1 0 0 0 0 1 1 1 1 1 3 3 3 3 1 1 1 0 0 0 0 0 1 1 1 1 3 3 3 3 1 1 1 0 0 0 0 0 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 13 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 13 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 3 3 1 1 1 1 0 0 0 0 1 1 1 1 1 3 3 3 3 1 1 1 0 0 0 0 1 1 1 1 1 3 3 3 3 1 1 1 0 0 0 0 1 1 1 1 1 3 3 3 3 1 1 1 0 0 0 0 0 1 1 1 1 3 3 3 3 1 1 1 0 0 0 0 0 1 1 1 1 1 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 16 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 3 3 1 1 1 1 1 1 15 15 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 1 1 1 1 1 1 15 15 1 1 1 1 1 3 3 3 1 1 1 1 1 1 15 15 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 0 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 0 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 1 0 1 1 1 1 1 1 1 1 3 3 3 3 3 3 1 0 1 1 1 1 1 1 1 1 1 3 3 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 1 0 1 1 1 1 1 1 1 16 3 3 3 3 3 1 1 0 1 1 1 1 1 1 1 16 16 3 3 1 1 1 1 0 13 1 1 1 1 1 1 1 16 16 16 1 1 1 1 0 0 1 1 1 1 1 1 1 1 16 16 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 16 16 0 0 0 0 1 1 1 1 1 1 1 1 1 1 16 16 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 16 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 1 1 0 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 0 1 1 1 1 1 1 1 16 16 3 3 1 1 1 1 0 1 1 1 1 1 1 1 16 16 16 1 1 1 1 1 0 13 1 1 1 1 1 1 1 16 16 16 1 1 1 1 0 0 1 1 1 1 1 1 1 1 16 16 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 16 16 1 0 0 0 0 1 1 1 1 1 1 1 1 1 16 16 16 0 0 0 0 1 1 1 1 1 1 1 1 1 1 16 16 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 16 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 16 16 16 0 13 1 1 1 1 1 1 1 1 1 1 1 1 1 16 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 16 16 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 16 16 0 0 0 0 1 1 1 1 1 1 1 1 1 1 16 16 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 16 16 16 0 13 1 1 1 1 1 1 1 1 1 1 1 1 1 16 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 13 13 13 1 1 1 1 1 1 1 1 1 1 1 1 0 13 13 13 13 13 13 1 1 1 1 1 1 1 1 1 0 0 13 13 13 13 13 13 1 1 1 1 1 1 1 1 0 0 1 1 13 13 13 13 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 16 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 3 1 1 1 1 13 13 13 1 1 1 1 1 1 1 1 1 1 1 1 1 0 13 13 13 13 13 1 1 1 1 1 1 1 1 1 1 0 13 13 13 13 13 13 13 1 1 1 1 1 1 1 1 0 0 13 13 13 13 13 13 13 1 1 1 1 1 1 1 1 0 13 13 13 13 13 13 13 1 1 1 1 1 1 1 0 0 1 1 13 13 13 13 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 15 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 16 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 16 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 13 13 13 1 1 1 1 1 1 1 3 3 3 1 1 1 0 13 13 13 13 13 1 1 1 1 1 3 3 1 1 1 0 13 13 13 13 13 13 13 1 1 1 1 1 1 1 1 0 0 13 13 13 13 13 13 13 1 1 1 1 1 3 1 1 0 13 13 13 13 13 13 13 1 1 1 1 1 1 1 0 0 1 1 13 13 13 13 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 15 15 0 0 0 0 1 1 1 1 1 1 1 1 1 1 15 15 0 0 0 0 0 1 13 13 1 1 1 1 1 1 1 1 0 0 0 0 0 13 13 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 16 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 15 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 1 0 13 13 13 1 1 1 1 1 1 1 3 3 1 1 1 0 13 13 13 13 13 13 1 1 1 1 1 1 1 3 3 1 0 13 13 13 13 13 13 1 16 16 1 3 3 3 3 1 1 1 1 13 13 13 13 1 1 16 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 13 13 1 1 1 1 1 1 15 1 0 0 0 0 0 13 13 13 13 1 1 1 1 1 1 1 0 0 0 0 0 13 13 13 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 15 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 15 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 1 1 1 1 1 1 1 16 16 1 3 3 3 3 1 1 1 1 1 1 1 1 1 16 16 3 3 3 3 3 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 0 0 0 1 1 1 1 1 1 3 3 3 3 1 1 1 0 0 0 0 1 1 13 13 1 1 1 1 1 1 1 1 0 0 0 0 0 13 13 13 1 1 1 1 1 1 1 1 0 0 0 0 0 13 13 13 1 1 1 1 1 1 1 1 )

   (vector
    1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 15 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 13 13 1 1 1 1 1 1 1 1 3 3 3 3 3 13 13 13 1 1 1 1 1 1 1 1 3 3 3 3 3 13 13 13 1 1 1 1 1 1 1 1 3 3 3 13 13 13 1 1 1 1 1 1 1 1 1 1 1 1 13 13 13 13 1 1 1 1 1 1 1 1 13 1 1 1 1 16 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 13 13 1 1 1 1 1 1 1 1 1 3 3 3 3 13 13 13 1 1 1 1 1 1 1 1 3 3 3 3 3 13 13 13 1 1 1 1 1 1 1 1 3 3 3 3 13 13 13 13 1 1 1 1 1 1 1 1 3 3 13 13 13 13 13 13 1 1 1 1 1 1 1 1 1 13 13 13 13 13 1 1 1 1 1 1 1 1 1 1 1 16 13 13 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 13 13 13 1 1 1 1 1 1 1 1 1 1 3 3 1 13 13 13 1 1 1 1 1 1 1 1 1 1 1 3 13 13 13 13 1 1 1 1 1 1 1 1 1 1 13 13 13 13 13 13 1 1 1 1 1 1 1 1 1 1 13 13 13 13 13 13 1 1 1 1 1 1 1 1 1 13 13 13 13 13 1 1 1 1 1 1 1 1 1 1 1 1 13 13 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 13 1 1 1 1 1 1 1 1 1 1 1 1 1 1 13 13 13 1 1 1 1 1 1 1 1 1 1 1 1 13 13 13 13 1 1 1 1 1 1 1 1 1 1 1 13 13 13 13 13 1 1 1 1 1 1 1 1 1 1 1 13 13 13 1 1 1 1 1 1 1 1 1 1 1 1 13 13 13 13 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 1 1 1 1 1 1 1 1 1 15 15 1 1 1 16 16 16 1 1 1 1 1 1 1 1 15 15 1 1 1 16 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 1 1 1 1 1 1 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 16 16 1 1 1 1 1 1 1 1 1 1 1 1 16 16 16 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 16 16 1 1 1 1 1 1 1 1 1 1 1 1 16 16 16 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 16 16 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
   
   (vector 0 0 0 0 0 1 1 1 15 15 15 1 1 1 1 0 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 1 1 1 1 1 1 1 16 16 1 1 1 3 3 3 3 1 1 1 1 1 1 16 16 16 16 1 1 1 1 3 1 1 1 1 1 1 1 1 16 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 16 16 1 1 1 1 3 1 1 1 1 1 1 1 1 1 16 16 1 1 1 3 3 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 12 0 3 3 3 3 3 3 3 3 1 1 1 1 16 16 1 3 3 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 16 16 1 1 1 1 1 1 1 1 1 1 1 1 1 1 16 1 1 1 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 15 15 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 12 0 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 12 3 3 3 3 3 3 3 3 1 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 1 1 1 1 1 3 3 3 3 3 3 3 1 3 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 15 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 15 1 1 1 1 1 1 1 1 1 1 3 1 3 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3 3 1 1 1 1 1 1 1 3 3 3 3 3 3 1 3 1 3 1 1 1 1 1 1 3 3 3 3 3 3 3 1 1 3 1 1 1 1 1 3 3 3 3 3 3 3 3 1 3 1 1 1 1 1 3 3 3 3 3 3 3 1 1 3 1 1 1 1 1 3 3 3 3 3 3 3 12 0 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 12 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 9 9 3 3 3 3 3 3 3 3 3 3 3 3 9 9 9 9 0 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 12 2 2 2 2 2 2 3 3 3 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 3 3 3 3 2 3 3 3 3 3 3 2 2 2 9 9 3 3 3 3 2 3 3 3 3 3 2 2 9 9 9 9 3 3 3 3 3 3 3 3 3 2 2 9 9 9 9 9 3 3 3 3 3 3 3 3 2 2 9 9 9 9 9 9 0 31 0 0 0 0 0 0 0 31 0 31 0 31 31 0 0 31 0 0 0 31 2 2 2 2 31 31 31 31 0 31 2 2 2 2 2 2 2 2 2 2 2 2 2 2 0 0 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 0 2 2 2 2 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 31 31 2 2 2 2 2 2 2 2 2 2 2 2 0 0 31 0 2 2 2 2 2 2 2 2 2 2 2 31 0 31 0 31 2 2 2 2 0 2 2 2 2 2 2 31 31 31 0 0 2 2 2 2 37 2 2 2 2 2 0 31 0 0 0 0 2 2 2 2 2 2 2 2 2 0 0 0 0 0 0 0 2 2 2 2 2 2 2 2 0 0 0 0 0 0 0 0 )
   (vector
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 31 31 0 0 0 0 0 0 0 0 0 0 31 0 0 0 0 0 31 31 0 31 0 0 0 0 31 0 31 31 0 31 0 31 0 0 31 31 0 0 0 0 0 0 2 2 0 31 0 31 31 31 0 31 0 31 0 31 0 0 0 0 0 0 0 0 31 0 31 31 31 0 31 0 0 0 0 0 0 31 31 0 31 31 0 31 31 31 0 0 0 0 0 31 31 0 31 31 31 31 31 31 31 31 0 0 0 0 0 31 0 31 0 37 0 0 31 0 31 31 0 0 0 0 31 31 31 0 31 31 31 0 0 0 0 0 0 0 31 0 0 31 31 37 31 31 0 31 0 0 0 0 0 0 0 0 0 0 0 37 31 31 31 0 0 0 0 0 0 0 0 0 0 31 0 31 0 31 0 0 0 0 0 0 0 0 0 0 0 37 0 0 0 31 0 0 0 0 0 0 0 0 37 0 0 0 0 0 31 0 0 0 0 0 0 0 0 0 31 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 31 31 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
