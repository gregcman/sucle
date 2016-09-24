(in-package :sandbox)

;;non-generic rendering

;;vaohash holds all the vaos which correlate to each chunk
(defparameter vaohash nil)
(defun render (camera)
  "responsible for rendering the world"
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (setf (simplecam-fov camera) 70)
  (set-matrix "view"
	      (mat:easy-lookat
	       (mat:add #2A((0 1.5 0 0))
			(simplecam-pos camera))
	       (simplecam-pitch camera)
	       (simplecam-yaw camera)))
  (set-matrix "projection"
	      (mat:projection-matrix
	       (deg-rad (simplecam-fov camera))
	       (/ out::pushed-width out::pushed-height) 0.01 128)) 
  (set-matrix "model" (mat:identity-matrix))
  (setf dirtychunks
	(sort
	 dirtychunks
	 (lambda (a b)
	   (<
	    (distoplayer a (simplecam-pos camera))
	    (distoplayer b (simplecam-pos camera))))))
  (setf dirtychunks (remove nil dirtychunks))
  (let ((achunk (pop dirtychunks)))
    (if achunk
	(if nil
	    (progn
	      (setf (gethash achunk vaohash) (shape-vao (chunk-shape (first achunk)
								     (second achunk)
								     (third achunk)))))
	    (progn
	      (if mesher-thread
		  (if (sb-thread:thread-alive-p mesher-thread)
		      (push achunk dirtychunks)
		      (multiple-value-bind (coords shape) (sb-thread:join-thread mesher-thread)
			(if shape
			    (setf (gethash coords vaohash) (shape-vao shape))
			    (progn
			      (push coords dirtychunks)))
			(if (> (+ (sqrt (* 3 16 16)) 128) (distoplayer achunk (simplecam-pos camera)))
			    (progn
			      (setf mesher-thread
				    (sb-thread:make-thread
				     (lambda (achunk)
				       (sb-thread:return-from-thread
					(values
					 achunk
					 (chunk-shape (first achunk)
						      (second achunk)
						      (third achunk)))))
				     :arguments (list achunk))))
			    (push achunk dirtychunks)))))))))

  (setf daytime (/ (+ 1 (sin (/ (get-internal-run-time) (* 20 60 1000)))) 2))
  (settime)
  (if (in:key-pressed-p #\g)
      (update-world-vao))
  (bind-shit "terrain.png")
  (maphash
   (lambda (key vao)
     (declare (ignore key))
     (draw-vao vao))
   vaohash)
  (gl:flush))

(defparameter daytime 1.0)

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

(defun glinnit ()
  "opengl initializing things"
  (setf mesher-thread (sb-thread:make-thread (lambda ())))
  (setf dirtychunks nil)
  (gl:enable :depth-test)
  (gl:disable :blend)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:active-texture :texture0)

  (setf vaohash (make-hash-table :test #'equal))
  
  (setq shaderProgram
	(load-and-make-shader
	 "transforms.vs" "basictexcoord.frag"))
  (gl:use-program shaderProgram))
