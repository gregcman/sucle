(in-package :sandbox)

;;non-generic rendering

;;vaohash holds all the vaos which correlate to each chunk
(defparameter vaohash nil)
(defun render (camera)
  "responsible for rendering the world"
  (gl:clear :color-buffer-bit :depth-buffer-bit)
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
  (setf dirtychunks (sort  dirtychunks
			   (lambda (a b) (<
					  (distoplayer a (simplecam-pos camera))
					  (distoplayer b (simplecam-pos camera))))))
  (setf dirtychunks (remove nil dirtychunks))
  (let ((achunk (pop dirtychunks)))	
    (if mesher-thread
	(if (sb-thread:thread-alive-p mesher-thread)
	    (if achunk
		(push achunk dirtychunks))
	    (multiple-value-bind (coords shape) (sb-thread:join-thread mesher-thread)
	      (if shape
		  (setf (gethash coords vaohash) (shape-vao shape))
		  (progn
		    (push coords dirtychunks)))
	      (progn
		(setf mesher-thread
		      (sb-thread:make-thread
		       (lambda (achunk)
			 (sb-thread:return-from-thread
			  (values
			   achunk
			   (chunk-shape achunk))))
		       :arguments (list achunk))))))))
  
  (if (in:key-pressed-p #\g)
      (update-world-vao))
  (maphash
   (lambda (key vao)
     (if (> (+ (sqrt (* 3 16 16)) 128) (distoplayer key (simplecam-pos camera)))
	 (progn
	   (set-matrix
	    "model"
	    (mat:translation-matrix
	     (* 16 (first key))(* 16 (second key)) (* 16 (third key))))
	   (draw-vao vao))))
   vaohash)
  (gl:flush))

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
     (pushnew k dirtychunks))
   chunkhash))

(defmacro progno (&rest nope))

(defun glinnit ()
  "opengl initializing things"
  (setf mesher-thread (sb-thread:make-thread (lambda ())))
  (if nil
      (gl:clear-color 0 0 0 1)
      (gl:clear-color 0.68 0.8 1.0 1.0))
  (gl:enable :depth-test)
  (gl:disable :blend)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:active-texture :texture0)
  
  (setq shaderProgram
	(load-and-make-shader
	 "transforms.vs" "basictexcoord.frag"))
  (gl:use-program shaderProgram))
