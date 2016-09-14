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
   vaohash)
  (gl:flush))

(defun update-world-vao ()
  "updates all of the vaos in the chunkhash. takes a long time"
  (maphash
   #'update-chunk-vao
   chunkhash))

(defmacro progno (&rest nope))

(defun update-chunk-vao (key &optional (chunk (gethash key chunkhash)))
  "updates the vao corresponding to a given chunk"
  (let ((old (gethash key vaohash)))
    (progno (if old
		(destroy-vao old)))
    (setf (gethash key vaohash)
	  (shape-vao (chunk-shape chunk key)))))

(defun glinnit ()
  "opengl initializing things"
  (if t
      (gl:clear-color 0 0 0 0)
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
