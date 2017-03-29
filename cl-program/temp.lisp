(in-package :sandbox)

(progno

 (defparameter +gl-primitives+
  (vector
   :points
   :lines
   :line-strip
   :line-loop
   :triangles
   :triangle-strip
   :triangle-fan
   :quads
   :quad-strip
   :polygon))
 (progn
   (defun draw-background (tex-buf pos-buf lit-buf)
     (declare (optimize (safety 0) (speed 3)))
     (declare (type iter-ator:iter-ator tex-buf pos-buf lit-buf))
     (iter-ator:wasabios ((etex tex-buf)
			  (epos pos-buf)
			  (elit lit-buf))
       (let ((distance +single-float-just-less-than-one+))
	 (etouq (ngorp (preach 'etex (duaq 1 nil '(0.0 1.0 0.0 1.0)))
		       (preach 'epos (quadk+ 'distance '(-1.0 1.0 -1.0 1.0)))
		       (preach 'elit (raps 1f0 4))))))
     4)

   (let ((%skybox-pos nil)
	 (%skybox-tex nil))
     (declare (type (or null simple-vector)
		    %skybox-pos %skybox-tex))
     (setf (values %skybox-pos %skybox-tex)
	   (let ((h0 0.0)
		 (h1 (/ 1.0 3.0))
		 (h2 (/ 2.0 3.0))
		 (h3 1.0)
		 (w0 0.0)
		 (w1 0.25)
		 (w2 0.5)
		 (w3 0.75)
		 (w4 1.0))
	     (let ((neg -10.0)
		   (pos 10.0))
	       (values (etouq (cons 'vector
				    (let ((npnp (quote (neg pos neg pos))))
				      (nconc (quadi+ 'neg npnp)
					     (quadi- 'pos npnp)
					     (quadj+ 'neg npnp)
					     (quadj- 'pos npnp)
					     (quadk+ 'neg npnp)
					     (quadk- 'pos npnp)))))
		       (etouq (cons 'vector
				    (nconc (duaq 2 nil '(w2 w3 h1 h2))
					   (duaq 3 nil '(w0 w1 h1 h2))
					   (duaq 2 nil '(w1 w2 h0 h1))
					   (duaq 1 nil '(w1 w2 h2 h3))
					   (duaq 1 nil '(w3 w4 h1 h2))
					   (duaq 4 nil '(w1 w2 h1 h2)))))))))
     (defun draw-skybox (tex-buf pos-buf lit-buf)
       (declare (optimize (safety 0) (space 3)))
       (declare (type iter-ator:iter-ator tex-buf pos-buf lit-buf))
       (iter-ator:wasabios ((etex tex-buf)
			    (epos pos-buf)
			    (elit lit-buf))
	 (dotimes (x (length %skybox-tex))
	   (etex (aref %skybox-tex x)))
	 (dotimes (x (length %skybox-pos))
	   (epos (aref %skybox-pos x)))
	 (dotimes (x 24)
	   (elit 1f0)))
       24)))

 (progno
  (defun set-sky-color ()
    (let ((r (* *daytime* (aref *sky-color* 0)))
	  (g (* *daytime* (aref *sky-color* 1)))
	  (b (* *daytime* (aref *sky-color* 2))))
      (gl:clear-color r g b 1.0)))
  (defparameter *daytime* 0.4)
  (defparameter *sky-color* (vector 1.0 0.8 0.68))
  (defparameter *fog-ratio* 0.75)
  (defun set-overworld-fog (time)
    (flet ((fractionalize (x)
	     (clamp x 0.0 1.0)))
      (let ((x (fractionalize (* (aref *sky-color* 0) time)))
	    (y (fractionalize (* (aref *sky-color* 1) time)))
	    (z (fractionalize (* (aref *sky-color* 2) time))))
	(%gl:uniform-3f (gl:get-uniform-location *shader-program* "fogcolor")
			x y z)
	(gl:uniformfv (gl:get-uniform-location *shader-program* "cameraPos")
		      (camera-vec-position *camera*))
	(%gl:uniform-1f (gl:get-uniform-location *shader-program* "foglet")
			(/ -1.0 (camera-frustum-far *camera*) *fog-ratio*))
	(%gl:uniform-1f (gl:get-uniform-location *shader-program* "aratio")
			(/ 1.0 *fog-ratio*))))))

 (progno
  
  (defparameter *framebuffer-width* 512)
  (defparameter *framebuffer-height* 512)
  (defparameter *framebuffer* nil)
  (defparameter *framebuffer-texture* nil)
  (bind-custom-framebuffer *framebuffer*)
  (gl:clear-color 0f0 1f0 0f0 1f0)
  (gl:clear :color-buffer-bit)

  (setf (values *framebuffer-texture* *framebuffer*)
	(create-framebuffer *framebuffer-width* *framebuffer-height*))
  (progno
   (progno
    (bind-custom-framebuffer *framebuffer*)
    (gl:viewport 0 0 *framebuffer-width* *framebuffer-height*))
   (progn
     (bind-default-framebuffer)))
  (defun clean-framebuffers ()
    (gl:delete-framebuffers-ext (list *framebuffer*))
    (gl:delete-textures (list *framebuffer-texture*))))
 
 (progno
  (gl:uniform-matrix-4fv
   (gl:get-uniform-location *shader-program* "projectionmodelview")
   (camera-matrix-projection-view-player *camera*)
   nil)
  
  (bind-shit :ocean)
  
  (ldrawlist :skybox))
 )

(progno
  (setf *camera* (make-camera))
   (setf (camera-aspect-ratio *camera*) (/ window:*width* window:*height* 1.0))
   (update-matrices *camera*)
   (defparameter *camera* nil)
   (progno
    (defun set-render-cam-pos (camera)
      (let ((vec (camera-vec-position camera))
	    (cev (camera-vec-noitisop camera)))
	(setf (aref vec 0) *xpos*)
	(setf (aref vec 1) *ypos*)
	(setf (aref vec 2) *zpos*)

	(setf (aref cev 0) (- *xpos*))
	(setf (aref cev 1) (- *ypos*))
	(setf (aref cev 2) (- *zpos*))

	(unit-pitch-yaw (camera-vec-forward *camera*)
			(coerce *pitch* 'single-float)
			(coerce *yaw* 'single-float))
	
	(setf (camera-fov *camera*) defaultfov)))

    (defun unit-pitch-yaw (result pitch yaw)
      (let ((cos-pitch (cos pitch)))
	(setf (aref result 0) (* cos-pitch (cos yaw))
	      (aref result 1) (sin pitch)
	      (aref result 2) (* cos-pitch (sin yaw))))
      result)))

(progno
     (name-shader :blockshader :bs-vs :bs-frag '(("position" . 0)
						 ("texCoord" . 2)
						 ("darkness" . 8)))
     (src-text :bs-vs (shader-path "blockshader/transforms.vs"))
     (src-text :bs-frag (shader-path "blockshader/basictexcoord.frag")))
