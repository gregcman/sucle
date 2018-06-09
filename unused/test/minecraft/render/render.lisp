
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
