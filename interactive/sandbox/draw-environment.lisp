(in-package :sandbox)

(defparameter *ourdir-aux* #.(or *compile-file-truename*
				 *load-truename*))
(defparameter ourdir
  (let ((value *ourdir-aux*))
    (make-pathname :host (pathname-host value)
		   :directory (pathname-directory value))))
(defparameter dir-resource (merge-pathnames #P"res/" ourdir))
(defparameter dir-shader (merge-pathnames #P"shaders/" dir-resource))
(defparameter dir-mc-assets (merge-pathnames "image/" dir-resource))
(defun shader-path (name)
  (merge-pathnames name dir-shader))
(defun img-path (name)
  (merge-pathnames name dir-mc-assets))


(defparameter *daytime* 1.0)
(defun draw-chunk-meshes ()
  (gl:enable :depth-test)  
  (gl:depth-func :less)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (draw-world))

(defun draw-world ()
  (declare (optimize (speed 3) (safety 0)))
  (with-hash-table-iterator (next *g/chunk-call-list*)
    (loop
       (multiple-value-bind (more? key value) (next)
	 (declare (ignore key))
	 (unless more? (return nil))
	 (gl:call-list value)))))
(progn
  (defparameter *g/chunk-call-list* (make-hash-table :test 'eq));;opengl call lists
  (defun get-chunk-display-list (name)
    (gethash name *g/chunk-call-list*))
  (defun set-chunk-display-list (name list-num)
    (setf (gethash name *g/chunk-call-list*) list-num))
  (defun remove-chunk-display-list (name)
    (remhash name *g/chunk-call-list*)))

(defun update-world-vao (x y z)
  (clean-dirty)
  (maphash (lambda (k v)
	     (declare (ignorable k))
	     (gl:delete-lists v 1)
	     (remove-chunk-display-list k))
	   *g/chunk-call-list*)
  (map nil #'dirty-push
       (sort (alexandria:hash-table-keys world::chunkhash) #'< :key
	     (lambda (position)
	       (multiple-value-bind (i j k) (world:unhashfunc position)
		 ((lambda (x0 y0 z0 x1 y1 z1)
		    (let ((dx (- x1 x0))
			  (dy (- y1 y0))
			  (dz (- z1 z0)))
		      (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
		  x y z
		  (- i 8)
		  (- k 8)
		  (- j 8)))))))


;;;the crosshair does not belong in the hud because the blending is
;;;different
#+nil
(defun draw-crosshair ()
  (bind-shit :gui)
  (gl:blend-func :one-minus-dst-color :one-minus-src-color)
  (ldrawlist :crosshair))

#+nil
;;matrix multiplication is associative
(defparameter *temp-matrix2* (cg-matrix:identity-matrix))
#+nil(defparameter *temp-matrix* (cg-matrix:identity-matrix))
;;;opengl stored matrices the transpose of sb-cga
#+nil
(defun draw-fist (camera)
  (gl:line-width 30.0)
  (set-matrix
   "projectionmodelview"
   (cg-matrix:%transpose-matrix
    *temp-matrix*
    (cg-matrix:%matrix*
     *temp-matrix2*
     (camera-matrix-projection-view-player camera)
     (cg-matrix:%translate*
      *temp-matrix*
      (coerce fist-side-x 'single-float)
      (coerce fist-side-y 'single-float)
      (coerce fist-side-z 'single-float)))))
  (gl:color 0.0 0.0 0.0)
  (gl:disable :cull-face :blend)
  (gl:polygon-mode :front-and-back :line)
  (let ((fist-pos (load-time-value
		   (make-array 3 :element-type (quote single-float)
			       :initial-contents (quote
						  (0.0 0.0 0.0))))))   
    (set-vec3 "fogcolor" fist-pos))
  (ldrawlist :selected-box)
  (gl:polygon-mode :front-and-back :fill))


#+nil
(defun on-resize (w h)
  (setf *window-height* h
	*window-width* w)
  (lcalllist-invalidate :gui)
  (lcalllist-invalidate :hotbar-selector)
  (lcalllist-invalidate :crosshair)
  (clean-framebuffers)
  (set-framebuffer))

#+nil
(progn
  (defparameter *crosshair-size* 20.0)
  (defparameter *hotbar-box-size* (* 22 4)))

#+nil
(progno
 (when fist?
   (draw-fist *camera*)
   )

 (progn
   (gl:disable :cull-face) 
   (luse-shader :solidshader)
   (set-matrix "projectionmodelview" cg-matrix:+identity-matrix+)

   (progn
     (draw-framebuffer)
     (draw-crosshair)
     (draw-hud)
     )))

#+nil
(defun draw-framebuffer ()
  (gl:enable :blend)
  (gl:depth-func :always)
  (gl:bind-texture :texture-2d *framebuffer-texture*)
  (ldrawlist :background))

#+nil
(defun draw-hud ()
  (bind-custom-framebuffer)
  (gl:clear-color 0.0 0.0 0.0 0.0)
;;  (gl:clear :color-buffer-bit)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (bind-shit :gui)
  (ldrawlist :gui)
  (ldrawlist :hotbar-selector))


#+nil
(progno
 (defun load-some-images ()
   #+nil
   (src-image "misc/grasscolor.png" (img-path #P"misc/grasscolor.png"))
   (src-image "gui/gui.png" (img-path #P"gui/gui.png"))
   #+nil
   (src-image "skybox/cheap.png" (img-path #P"skybox/cheap.png"))
   #+nil
   (src-image "terrain/sun.png" (img-path #P"terrain/sun.png"))
   #+nil
   (src-image "terrain/moon.png" (img-path #P"terrain/moon.png"))
   )

 (defun texture-imageries ()
   #+nil
   (texture-imagery :skybox "skybox/cheap.png")
   #+nil
   (texture-imagery :sun "terrain/sun.png")
   #+nil
   (texture-imagery :moon "terrain/moon.png")
   #+nil
   (texture-imagery :gui "gui/gui.png"))
 (defun name-shaders ()
   #+nil

   (name-shader :solidshader :ss-vs :ss-frag '(("position" . 0)
					       ("texCoord" . 2)

					       ("darkness" . 8)))
   #+nil
   (defun name-funcs ()
     (name-mesh :skybox #'draw-skybox)
     (name-mesh :sun #'draw-sun)
     (name-mesh :moon #'draw-moon)
     (name-mesh :selected-box
		(lambda () (let ((foo 0.005))
			     (let ((min (- 0.0 foo))
				   (max (+ 1.0 foo)))
			       (draw-box min min min max max max)))))
     (name-mesh :background #'draw-background)
     (name-mesh :crosshair #'mesh-crosshair)
     (name-mesh :gui #'draw-hotbar)
     (name-mesh :hotbar-selector #'draw-hotbar-selector))
))

#+nil
((defun draw-box (minx maxx miny maxy minz maxz)
   (let ((iter aplayground::*attrib-buffer-iterators*))
     (aplayground::reset-attrib-buffer-iterators iter)
     (let ((buf (aplayground::get-buf-param
		 iter
		 (etouq (vector 0 2 8)))))
       (let ((len 0))
	 (aplayground::with-iterators (epos etex dark)
	     buf iter-ator:wasabios
	   (etouq
	    (aplayground::ngorp
	     (aplayground::preach
	      'epos
	      (nconc
	       (axis-aligned-quads:quadi+
		'maxx
		'(miny maxy minz maxz))
	       (axis-aligned-quads:quadi-
		'minx
		'(miny maxy minz maxz))
	       (axis-aligned-quads:quadj+
		'maxy
		'(minx maxx minz maxz))
	       (axis-aligned-quads:quadj-
		'miny
		'(minx maxx minz maxz))
	       (axis-aligned-quads:quadk+
		'maxz
		'(minx maxx miny maxy))
	       (axis-aligned-quads:quadk-
		'minz
		'(minx maxx miny maxy))))))
	   (flet ((wot (x)
		    (dotimes (i 4)
		      (dark x))))
	     (etouq
	      (aplayground::ngorp
	       (aplayground::preach
		'wot '(0.6 0.6 1.0 0.5 0.8 0.8)))))
	   (dotimes (x 6)
	     (etouq
	      (aplayground::ngorp
	       (aplayground::preach
		'etex
		(axis-aligned-quads:duaq 1 t (let ((a (/ 0.0 16))
						   (b (/ 15.0 16)))
					       `(,a ,(+ a (/ 1.0 16.0))
						    ,b
						    
						    ,(+ b (/ 1.0 16.0)))))))))
	   (setf len 24))
	 (aplayground::reset-attrib-buffer-iterators iter)
	 (let ((list (gl:gen-lists 1)))
	   (gl:new-list list :compile)
	   (gl:with-primitives :quads
	     (mesh-chunk len buf))
	   (gl:end-list)
	   list)))))

 (defun compute-entity-aabb-matrix (entity partial)
   (let ((aabb (entity-aabb entity))
	 (pos (farticle-position (entity-particle entity)))
	 (posold (farticle-position-old (entity-particle entity))))
     (let ((avgx (/ (+ (aabbcc:aabb-minx aabb)
		       (aabbcc:aabb-maxx aabb))
		    2.0))
	   (avgy (/ (+ (aabbcc:aabb-miny aabb)
		       (aabbcc:aabb-maxy aabb))
		    2.0))
	   (avgz (/ (+ (aabbcc:aabb-minz aabb)
		       (aabbcc:aabb-maxz aabb))
		    2.0))
	   (difx (/ (- (aabbcc:aabb-minx aabb)
		       (aabbcc:aabb-maxx aabb))
		    -2.0))
	   (dify (/ (- (aabbcc:aabb-miny aabb)
		       (aabbcc:aabb-maxy aabb))
		    -2.0))
	   (difz (/ (- (aabbcc:aabb-minz aabb)
		       (aabbcc:aabb-maxz aabb))
		    -2.0)))
       (let ((pos2 (cg-matrix:vec avgx avgy avgz)))
	 (cg-matrix:matrix* (cg-matrix:translate (cg-matrix:vec+ pos2
								 (cg-matrix:vec-lerp posold pos partial)))
			    (cg-matrix:scale* difx dify difz)))))))


#+nil
((defparameter *fog-ratio*  0.75)
 (defparameter *avector* (cg-matrix:vec 0.0 0.0 0.0))
 (defparameter *fogcolor* (apply #'cg-matrix:vec
				 (nth 0 '((0.68 0.8 1.0)
					  (0.05 0.1 0.2)
					  (0.3 0.1 0.0))))))

