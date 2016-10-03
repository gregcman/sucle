(in-package :sandbox)

;;non-generic rendering

;;vaohash holds all the vaos which correlate to each chunk
(defparameter vaohash (make-hash-table :test #'equal))
(defparameter shaderhash (make-hash-table :test #'equal))
(defmacro toggle (a)
  `(setf ,a (not ,a)))
(defparameter drawmode nil)

(defun leresize (option)
  (out:push-dimensions option)
  (gl:viewport 0 0 out:width out:height))

(defun ease (x target fraction)
  (+ x (* fraction (- target x))))

(defun render ()
  "responsible for rendering the world"
  (let ((camera (getworld "player")))
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (if isprinting
	(setf (simplecam-fov camera)
	      (ease (simplecam-fov camera) 90 0.2))
	(setf (simplecam-fov camera)
	      (ease (simplecam-fov camera) 70 0.2))) 
    (setupmatrices camera)
    (sortdirtychunks camera)
    (designatemeshing)
    (settime)
    (if (in:key-pressed-p #\v)
	(progn (leresize t)))
    (if (in:key-pressed-p #\g)
	(update-world-vao))
    (if (in:key-pressed-p #\y)
	(loadblockshader))
    (if (in:key-pressed-p #\q)
	(progn
	  (toggle drawmode)
	  (if drawmode
	      (gl:polygon-mode :front-and-back :line)
	      (gl:polygon-mode :front-and-back :fill))))
    (draw-chunk-meshes)
    (gl:flush)))

(defun draw-chunk-meshes ()
  (gl:enable :depth-test)
  (gl:disable :blend)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:active-texture :texture0)
  (bind-shit "terrain.png")
  (use-program "blockshader")
  (set-matrix "model" (mat:identity-matrix))
  (maphash
   (lambda (key vao)
     (declare (ignore key))
     (draw-vao vao))
   vaohash))

(defun designatemeshing ()
  (if (string= mesherwhere? "worker")
      (if (mesherthreadbusy)
	  (progn)
	  (progn
	    (if mesher-thread
		(getmeshersfinishedshit))
	    (let ((achunk (car dirtychunks)))
	      (if achunk
		  (progn
		    (giveworktomesherthread achunk)
		    (setf dirtychunks
			  (delete achunk dirtychunks :test #'equal)))))))
      (let ((achunk (pop dirtychunks)))
	(if achunk
	    (setf (gethash achunk vaohash)
		  (shape-vao (chunk-shape (first achunk)
					  (second achunk)
					  (third achunk))))))))

(defun setupmatrices (camera)
  (set-matrix "view"
	      (mat:easy-lookat
	       (mat:add (mat:onebyfour
			 (list 0
			       (if isneaking (- 1.5 1/8) 1.5)
			       0 0))
			(simplecam-pos camera))
	       (simplecam-pitch camera)
	       (simplecam-yaw camera)))
  (set-matrix "projection"
	      (mat:projection-matrix
	       (deg-rad (simplecam-fov camera))
	       (/ out::pushed-width out::pushed-height) 0.01 128)) )

(defun sortdirtychunks (camera)
  (progn
    (setf dirtychunks
	  (remove nil
		  (sort
		   dirtychunks
		   (lambda (a b)
		     (<
		      (distoplayer a (simplecam-pos camera))
		      (distoplayer b (simplecam-pos camera)))))))))

(defparameter mesherwhere? (if t "worker" "main"))

(defun getmeshersfinishedshit ()
  (multiple-value-bind (coords shape) (sb-thread:join-thread mesher-thread)
    (if coords
	(if shape
	    (setf (gethash coords vaohash) (shape-vao shape))
	    (progn
	      (pushnew coords dirtychunks :test #'equal)))))
  (setf mesher-thread nil))

(defun mesherthreadbusy ()
  (not (or (eq nil mesher-thread)
	   (not (sb-thread:thread-alive-p mesher-thread)))))

(defun giveworktomesherThread (thechunk)
  (setf mesher-thread
	(sb-thread:make-thread
	 (lambda (achunk)
	   (sb-thread:return-from-thread
	    (values
	     achunk
	     (chunk-shape (first achunk)
			  (second achunk)
			  (third achunk)))))
	 :arguments (list thechunk))))

(defun settime ()
  (set-float "timeday" daytime)
  (setnight daytime))

(defun setnight (val)
  (let ((a (lightstuff val)))
    (gl:clear-color  
     (* a 0.68)
     (* a 0.8)
     (* a 1.0) 1.0)
    (set-vec4 "fogcolor"
	      (vector
	       (* a 0.68)
	       (* a 0.8)
	       (* a 1.0) 1.0))))

(defun lightstuff (num)
  (expt 0.8 (- 15 (* 15  num))))

(defparameter mesher-thread nil)

(defun distoplayer (keys pos)
  (hypot
   (diff (scalis keys 16) (mat-lis pos))))

(defun mat-lis (mat)
  (let ((thelist nil))
    (dotimes (x 3)
       (push (row-major-aref mat x) thelist))
    (nreverse thelist)))

(defun diff (a b)
  (mapcar (function -) a b))

(defun scalis (liz s)
  (mapcar (lambda (x) (* x s)) liz))

(defun hypot (list)
  (sqrt (apply (function +) (mapcar (lambda (x) (* x x)) list))))

(defun update-world-vao ()
  "updates all of the vaos in the chunkhash. takes a long time"
  (maphash
   (lambda (k v)
     (declare (ignore v))
     (pushnew (unchunkhashfunc k) dirtychunks :test #'equal))
   chunkhash))

(defmacro progno (&rest nope))

(defun use-program (name)
  (let ((ourprog (gethash name shaderhash)))
    (setq shaderProgram ourprog)
    (gl:use-program ourprog)))

(defun load-a-shader (name vs frag attribs)
  (setf (gethash name shaderhash)
	(load-and-make-shader
	 vs
	 frag
	 attribs)))

(defun glinnit ()
  "initializing things"
  (loadletextures)
  (load-into-texture-library "items.png")
  (load-into-texture-library "grasscolor.png")
  (load-into-texture-library "foliagecolor.png")
  (load-into-texture-library "terrain.png")
  (bind-shit "terrain.png")
  (setf dirtychunks nil)
  (setf mesher-thread nil)
  (clrhash vaohash)
  (clrhash shaderhash)
  (loadblockshader)
  (use-program "blockshader"))

(defun load-into-texture-library (name &optional (othername name))
  (let ((thepic (gethash name picture-library)))
    (if thepic
	(let ((dims (array-dimensions thepic)))
	    (load-shit
	     (fatten thepic)
	     othername (first dims) (second dims))))))

(defun loadblockshader ()
  (load-a-shader
   "blockshader"
   "transforms.vs"
   "basictexcoord.frag"
   '(("position" . 0)
     ("texCoord" . 2)
     ("color" . 4)
     ("blockLight" . 8)
     ("skyLight" . 9))))
