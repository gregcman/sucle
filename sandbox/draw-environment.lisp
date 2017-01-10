(in-package :sandbox)

(defmacro vvv (darkness u v x y z)
  `(progn (gl:vertex-attrib 8 ,darkness)
	  (gl:vertex-attrib 2 ,u ,v)
	  (gl:vertex-attrib 0 ,x ,y ,z)))

(defun draw-background ()
  (glshader:set-matrix "projectionmodelview"
		       sb-cga:+identity-matrix+)
  (let ((distance 0.99999997))
    (gl:with-primitives :quads
      (vvv 1.0 0.0 1.0 -1.0 -1.0 distance)
      (vvv 1.0 1.0 1.0 -1.0 -1.0 distance)
      (vvv 1.0 1.0 0.0 1.0 1.0 distance)
      (vvv 1.0 0.0 0.0 -1.0 1.0 distance))))

(defun draw-skybox ()
  (let ((h0 0.0)
	(h1 (/ 1.0 3.0))
	(h2 (/ 2.0 3.0))
	(h3 (/ 3.0 3.0))
	(w0 0.0)
	(w1 (/ 1.0 4.0))
	(w2 (/ 2.0 4.0))
	(w3 (/ 3.0 4.0))
	(w4 (/ 4.0 4.0)))
    (let ((neg -20.0)
	  (pos 20.0))
      (gl:with-primitives :quads
	;;j+
	(vvv 1.0 w2 h3 neg pos neg)
	(vvv 1.0 w2 h2  pos pos neg)
	(vvv 1.0 w1 h2  pos pos pos)
	(vvv 1.0 w1 h3 neg pos pos)

	;;j-
	(vvv 1.0 w2 h0 neg neg neg)
	(vvv 1.0 w1 h0 neg neg pos)
	(vvv 1.0 w1 h1 pos neg pos)
	(vvv 1.0 w2 h1 pos neg neg)

	;;k-
	(vvv 1.0 w3 h2 neg pos neg)
	(vvv 1.0 w3 h1 neg neg neg)
	(vvv 1.0 w2 h1 pos neg neg)
	(vvv 1.0 w2 h2 pos pos neg)

	;;k+
	(vvv 1.0 w1 h1 pos neg pos)
	(vvv 1.0 w0 h1 neg neg pos)
	(vvv 1.0 w0 h2 neg pos pos)
	(vvv 1.0 w1 h2 pos pos pos)
	
	;;i-
	(vvv 1.0 w3 h1 neg neg neg)
	(vvv 1.0 w3 h2 neg pos neg)
	(vvv 1.0 w4 h2 neg pos pos)
	(vvv 1.0 w4 h1 neg neg pos)

	;;i+
	(vvv 1.0 w2 h1 pos neg neg)
	(vvv 1.0 w1 h1 pos neg pos)
	(vvv 1.0 w1 h2 pos pos pos)
	(vvv 1.0 w2 h2 pos pos neg)

	))))

(defun draw-sun ()
  (let ((distance 5.0))
    (gl:with-primitives :quads
      (vvv 1.0 1.0 1.0  1.0 -1.0 distance)
      (vvv 1.0 1.0 0.0 -1.0 -1.0 distance)
      (vvv 1.0 0.0 0.0 -1.0  1.0 distance)
      (vvv 1.0 0.0 1.0  1.0  1.0 distance))))

(defun draw-moon ()
  (let ((distance -5.0))
    (gl:with-primitives :quads
      (vvv 1.0 1.0 1.0  -1.0 1.0 distance)
      (vvv 1.0 1.0 0.0 -1.0 -1.0 distance)
      (vvv 1.0 0.0 0.0 1.0  -1.0 distance)
      (vvv 1.0 0.0 1.0  1.0  1.0 distance))))

(defparameter *call-list)

(defun draw-sky ()
  (glshader:set-matrix "projectionmodelview"
		       (sb-cga:transpose-matrix
			*projection-view-matrix*))
  (gl:depth-func :always)
  
  (bind-shit :skybox)
  (draw-skybox)

  (let ((time (daytime)))
    (glshader:set-matrix "projectionmodelview"
			 (sb-cga:transpose-matrix
			  (sb-cga:matrix*
			   *projection-view-matrix*
			   (sb-cga:rotate-around
			    (sb-cga:vec -1.0 0.0 0.0)
			    time))))

    (gl:enable :blend)
    (gl:blend-func :src-alpha :src-alpha)
    (bind-shit :sun)
    (unless (lget *g/call-list* :sun)
      (lcreate-call-list #'draw-sun :sun))
    (ldrawlist :sun)
    
    (bind-shit :moon)
    (unless (lget *g/call-list* :moon)
      (lcreate-call-list #'draw-moon :moon))
    (ldrawlist :moon)
    
    (gl:disable :blend)))

(defun fractionalize (x)
  (clamp x 0.0 1.0))

(defun set-overworld-fog (time)
  (let ((x (fractionalize (* time 0.68)))
	(y (fractionalize (* time 0.8)))
	(z (fractionalize (* time 1.0)))
	(w 1.0))
    (gl:clear-color x y z w)
    (glshader:set-vec3 "fogcolor"
	      (vector x y z w))))

(defun daytime ()
  (coerce (* (get-internal-run-time)
	     (/ 840.0 100000000.0))
	  'single-float))


(defun create-call-list-from-func (func)
  (let ((the-list (gl:gen-lists 1)))
    (gl:new-list the-list :compile)
    (funcall func)
    (gl:end-list)
    the-list))

(defun lcreate-call-list (func name)
  (let ((the-list (create-call-list-from-func func)))
    (let ((old (lget *g/call-list* name)))
      (lset *g/call-list* name the-list)
      (when old
	(gl:delete-lists old 1)))))

(defun ldrawlist (name)
  (let ((the-list (lget *g/call-list* name)))
    (if the-list
	(gl:call-list the-list)
	(print "error"))))
