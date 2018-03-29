 
(defun draw-fistbox ()
  ;;;;draw the fist hitbox
  (progn
    (with-slots ((minx aabbcc::minx) (miny aabbcc::miny) (minz aabbcc::minz)
		      (maxx aabbcc::maxx) (maxy aabbcc::maxy) (maxz aabbcc::maxz))
	     fist-aabb
	   (draw-box (+ minx fistx -0) (+  miny fisty -0) (+  minz fistz -0)
		     (+ maxx fistx -0) (+  maxy fisty -0) (+  maxz fistz -0)))))

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
