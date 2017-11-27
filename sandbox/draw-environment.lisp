(in-package :sandbox)

(progn
  (defparameter *g/call-list* (make-hash-table :test 'eq));;opengl call lists
  (defun get-display-list (name)
    (let ((display-list (gethash name *g/call-list*)))
      (if display-list
	  display-list
	  (get-display-list-backup name))))
  (defun set-display-list (name list-num)
    (setf (gethash name *g/call-list*) list-num))
  (defun remove-display-list (name)
    (remhash name *g/call-list*)))
(progn
  (defparameter *g/call-list-backup* (make-hash-table :test 'eq))
  (defun get-display-list-backup (name)
    (let ((display-list-func (gethash name *g/call-list-backup*)))
      (when (functionp display-list-func)
	(let ((ans (funcall display-list-func name)))
	  (when ans
	    (set-display-list name ans)))))))

(defparameter *fog-ratio*  0.75)

(defparameter *avector* (cg-matrix:vec 0.0 0.0 0.0))
(defparameter *fogcolor* (apply #'cg-matrix:vec
				(nth 0 '((0.68 0.8 1.0)
					 (0.3 0.1 0.0)))))
(defparameter *daytime* 1.0)

(defun fractionalize (x)
  (alexandria:clamp x 0.0 1.0))
(defun render (camera deps partial)
  (declare (optimize (safety 3) (debug 3)))
  (flet ((getfnc (name)
	   (funcall deps name)))
    (let* ((blockshader (getfnc :blockshader))
	   (blockshader-uniforms *blockshader-uniforms*)
	   (fogcolor (aplayground::getuniform blockshader-uniforms :fog-color))
	   (aratio (aplayground::getuniform blockshader-uniforms :aratio))
	   (foglet (aplayground::getuniform blockshader-uniforms :foglet))
	   (pmv (aplayground::getuniform blockshader-uniforms :pmv))
	   (cam-pos (aplayground::getuniform blockshader-uniforms :cam-pos)))
      (gl:use-program blockshader)
      (gl:uniformfv cam-pos (camera-vec-position camera))
      (gl:uniform-matrix-4fv
       pmv
       (camera-matrix-projection-view-player camera)
       nil)
      
      (let ((time *daytime*)
	    (avector *avector*))
	(map-into avector
		  (lambda (x)
		    (fractionalize (* time x)))
		  *fogcolor*)
	(gl:clear-color (aref avector 0) (aref avector 1) (aref avector 2) 1.0)
	(gl:uniformfv fogcolor avector))
      (gl:uniformf foglet (/ -1.0 (camera-frustum-far camera) *fog-ratio*))
      (gl:uniformf aratio (/ 1.0 *fog-ratio*))
      
      (gl:disable :blend)

  ;;;static geometry with no translation whatsoever
      ;; (sandbox::bind-default-framebuffer)
      (gl:bind-texture
       :texture-2d
       (getfnc :terrain))
      (draw-chunk-meshes)


      #+nil
      (progno
	(dotimes (x (length fuck::*ents*))
	  (let ((aaah (aref fuck::*ents* x)))
	    (unless (eq aaah fuck::*ent*)
	      (gl:uniform-matrix-4fv
	       pmv
	       (cg-matrix:matrix* (camera-matrix-projection-view-player camera)
				  (compute-entity-aabb-matrix aaah partial))
	       nil)
	      (gl:call-list (getfnc :box)))))))
    (designatemeshing)))

(defun compute-entity-aabb-matrix (entity partial)
  (let ((aabb (entity-aabb entity))
	(pos (farticle-position (entity-particle entity)))
	(posold (farticle-position-old (entity-particle entity))))
    (let ((avgx (/ (+ (aabbcc::aabb-minx aabb)
		      (aabbcc::aabb-maxx aabb))
		   2.0))
	  (avgy (/ (+ (aabbcc::aabb-miny aabb)
		      (aabbcc::aabb-maxy aabb))
		   2.0))
	  (avgz (/ (+ (aabbcc::aabb-minz aabb)
		      (aabbcc::aabb-maxz aabb))
		   2.0))
	  (difx (/ (- (aabbcc::aabb-minx aabb)
		      (aabbcc::aabb-maxx aabb))
		     -2.0))
	  (dify (/ (- (aabbcc::aabb-miny aabb)
		      (aabbcc::aabb-maxy aabb))
		   -2.0))
	  (difz (/ (- (aabbcc::aabb-minz aabb)
		      (aabbcc::aabb-maxz aabb))
		   -2.0)))
      (let ((pos2 (cg-matrix:vec avgx avgy avgz)))
	(cg-matrix:matrix* (cg-matrix:translate (cg-matrix:vec+ pos2
								(cg-matrix:vec-lerp posold pos partial)))
			   (cg-matrix:scale* difx dify difz))))))

#+nil
(defparameter *velocity* (cg-matrix:vec 0.0 0.0 0.0))
#+nil
(defparameter *orientation* (make-array 6 :element-type 'single-float
					:initial-contents
					'(0.0 0.0 0.0 0.0 0.0 0.0)))

(defun draw-chunk-meshes ()
  (gl:enable :depth-test)  
  (gl:depth-func :less)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (let ((call-list
	 (get-display-list :world)))
    (if call-list    
	(gl:call-list call-list)
	(let ((new (gl:gen-lists 1)))
	  (gl:new-list new :compile)
	  (draw-world)
	  (gl:end-list)
	  (set-display-list :world new)
	  (gl:call-list new)))))
(defun draw-world ()
  (maphash
   (lambda (key display-list)
     (when (numberp key)
       (gl:call-list display-list)))
   *g/chunk-call-list*))
(progn
  (defparameter *g/chunk-call-list* (make-hash-table :test 'eq));;opengl call lists
  (defun get-chunk-display-list (name)
    (gethash name *g/chunk-call-list*))
  (defun set-chunk-display-list (name list-num)
    (setf (gethash name *g/chunk-call-list*) list-num))
  (defun remove-chunk-display-list (name)
    (remhash name *g/chunk-call-list*)))

(defun distance-to (x0 y0 z0 x1 y1 z1)
  (let ((dx (- x1 x0))
	(dy (- y1 y0))
	(dz (- z1 z0)))
    (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
(defun update-world-vao (x y z)
  (clean-dirty)
  (maphash (lambda (k v)
	     (declare (ignorable k))
	     (gl:delete-lists v 1)
	     (remove-chunk-display-list k))
	   *g/chunk-call-list*)
  (let ((list nil))
    (maphash
     (lambda (k v)
       (declare (ignore v))
       (push k list))
     world::chunkhash)
    (dolist (x (sort list #'< :key
		     (lambda (position)
		       (multiple-value-bind (i j k) (world:unhashfunc position)
			 (distance-to x y z
				      (- i 8)
				      (- k 8)
				      (- j 8))))))
      (dirty-push x))))

(defparameter ourdir
  (make-pathname :host (pathname-host #.(or *compile-file-truename*
					    *load-truename*))
		 :directory (pathname-directory #.(or *compile-file-truename*
						      *load-truename*))))
(defparameter dir-resource (merge-pathnames #P"res/" ourdir))
(defparameter dir-shader (merge-pathnames #P"shaders/" dir-resource))
(defparameter dir-mc-assets (merge-pathnames "image/" dir-resource))
(defun shader-path (name)
  (merge-pathnames name dir-shader))
(defun img-path (name)
  (merge-pathnames name dir-mc-assets))

(progn
  (defun color-grasses (image terrain)
    (let ((color (case 1
		   (0 #(1742848/8775 2673664/8775 1079296/8775 255))
		   (1 (getapixel 255 0 image))
		   (2 (getapixel 0 0 image))
		   (3 (getapixel 255 255 image)))))
      (modify-greens 64 192 :color color :terrain terrain)
      (modify-greens 80 192 :color color :terrain terrain)
      (modify-greens 0 240 :color color :terrain terrain)))

  (defun ubyte-mult (a b)
    (truncate (* a b) 256))

  (defun multiply-into (vecinto other)
    (macrolet ((aux (a b num)
		 `(let ((at (aref ,a ,num))
			(bt (aref ,b ,num)))
		    (setf (aref ,a ,num) (ubyte-mult at bt)))))
      (aux vecinto other 0)
      (aux vecinto other 1)
      (aux vecinto other 2)
      (aux vecinto other 3)))

  (defun getapixel (h w image)
    (destructuring-bind (height width c) (array-dimensions image)
      (declare (ignore height))
      (make-array 4 :element-type (array-element-type image)
		  :displaced-to image
		  :displaced-index-offset (* c (+ w (* h width))))))

  (progno #(113 174 70 255)  #(198 304 122 255))
;;;grass is 0 240
;;;leaves is [64 80] 192
  (defun modify-greens (xpos ypos
			&key
			  (color #(0 0 0 0))
			  (terrain (error "no image")))
    (dobox ((x xpos (+ 16 xpos)) (y ypos (+ 16 ypos)))
	   (multiply-into (getapixel y x terrain) color))))

(defun build-deps (getfnc setfnc)
  (flet ((bornfnc (name func)
	   (funcall setfnc name func))
	 (getfnc (name)
	   (funcall getfnc name)))
    (progn
      (bornfnc
       :terrain-png
       (lambda ()
	 (let ((image
		(aplayground::flip-image
		 (aplayground::load-png 
		  (img-path #P"terrain.png")))))
	   (color-grasses
	    (getfnc :grass-png)
	    image)
	   image)))
      (bornfnc
       :grass-png
       (lambda ()
	 (aplayground::load-png 
	  (img-path #P"grasscolor.png"))))
      (bornfnc
       :terrain
       (lambda ()
	 (prog1
	     (lovely-shader-and-texture-uploader:pic-texture
	      (getfnc :terrain-png)
	      :rgba)
					;	 (gl:generate-mipmap :texture-2d)
	   (lovely-shader-and-texture-uploader::apply-tex-params
	    (quote ((:texture-min-filter . :nearest;-mipmap-nearest
					 )
		    (:texture-mag-filter . :nearest)
		    (:texture-wrap-s . :repeat)
		    (:texture-wrap-t . :repeat)))))))
      (bornfnc
       :blockshader
       (lambda ()
	 (let ((program
		(lovely-shader-and-texture-uploader:make-shader-program-from-strings
		 (getfnc :bs-vs)
		 (getfnc :bs-frag)
		 (quote (("position" . 0)	
			 ("texCoord" . 2)
			 ("darkness" . 8)
			 )))))
	   (let ((table (make-hash-table :test 'eq)))
	     (aplayground::cache-program-uniforms
	      program
	      table
	      (quote ((:pmv . "projectionmodelview")
		      (:fog-color . "fogcolor")
		      (:aratio . "aratio")
		      (:cam-pos . "cameraPos")
		      (:foglet . "foglet")
		      )))
	     (Setf *blockshader-uniforms* table))
	   program)))
      (bornfnc
       :bs-vs
       (lambda ()
	 (alexandria:read-file-into-string
		   (shader-path "blockshader/transforms.vs"))))
      (bornfnc
       :bs-frag
       (lambda ()
	 (alexandria:read-file-into-string
	  (shader-path "blockshader/basictexcoord.frag"))))
      (bornfnc
       :box
       (lambda ()
	 (draw-box -1.0 1.0 -1.0 1.0 -1.0 1.0))))))


(defparameter *blockshader-uniforms* nil)

#+nil
(in-package :sandbox)

;;;various box sizes for different things

;;;its a cubic meter
#+nil
(defun block-aabb ()
  )

;;;a person's personal space
#+nil
(defun player-aabb ()
  )

#+nil
(defun player-aabb+1 ()
  (aabbcc::make-aabb
   :minx -0.3
   :miny -0.5
   :minz -0.3
   :maxx 0.3
   :maxy 1.12
   :maxz 0.3))

#+nil
(defun chunk-aabb ()
  (aabbcc::make-aabb
   :minx -8.0
   :miny -8.0
   :minz -8.0
   :maxx 8.0
   :maxy 8.0
   :maxz 8.0))

#+nil
(defparameter chunk-aabb (chunk-aabb))
#+nil
(defparameter player-aabb+1 (player-aabb+1))

#+nil
(defun fist-aabb ())

#+nil
(defparameter player-aabb (player-aabb))
#+nil
(defparameter *fist-aabb*
  )

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
(defun texture-imagery (texture-name image-name)
  (setf (gethash texture-name *g/texture-backup*)
	(lambda (&optional name)
	  (declare (ignorable name))
	  (pic-texture (get-image image-name)))))

#+nil
(name-shader :blockshader :bs-vs :bs-frag '(("position" . 0)
					    ("texCoord" . 2)
					    ("darkness" . 8)))
#+nil
(defun name-shader (shader-name vs fs attributes)
  (setf (gethash shader-name *g/shader-backup*)
	(lambda (&optional name)
	  (declare (ignorable name))
	  (make-shader-program-from-strings
	   (get-text vs) (get-text fs) attributes))))


#+nil
(defun src-text (name src-path)
  (setf (gethash name *g/text-backup*)
	(lambda (&optional name)
	  (declare (ignorable name))
	  (aplayground::file-string src-path))))

#+nil
(defun src-image (name src-path)
  (setf (gethash name *g/image-backup*)
	(lambda (&optional name)
	  (declare (ignorable name))
	  (let ((img (imagewise:load-png src-path)))
	    (imagewise:flip-image img)
	    img))))

#+nil
(defun name-mesh (display-list-name mesh-func)
  (setf (gethash display-list-name *g/call-list-backup*)
	(lambda (&optional name)
	  (declare (ignorable name))
	  (create-call-list-from-func mesh-func))))

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
(progno
 (defparameter *vec4* (make-array 4 :element-type 'single-float))
 (defun vec4 (vec3)
   (setf (aref *vec4* 0) (aref vec3 0))
   (setf (aref *vec4* 1) (aref vec3 1))
   (setf (aref *vec4* 2) (aref vec3 2))
   *vec4*))


#+nil
(defun draw-framebuffer ()
  (gl:enable :blend)
  (gl:depth-func :always)
  (gl:bind-texture :texture-2d *framebuffer-texture*)
  (ldrawlist :background))

;;;the crosshair does not belong in the hud because the blending is
;;;different
#+nil
(defun draw-crosshair ()
  (bind-shit :gui)
  (gl:blend-func :one-minus-dst-color :one-minus-src-color)
  (ldrawlist :crosshair))

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

   (defun load-shaders ())
   #+nil
   (src-text :ss-vs (shader-path "solidshader/transforms.vs"))
   #+nil
   (src-text :ss-frag (shader-path "solidshader/basictexcoord.frag"))))

#+nil
(defun glinnit ()
  )

(defun draw-box (minx maxx miny maxy minz maxz)
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
