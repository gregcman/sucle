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

(defparameter ourdir
  (make-pathname :host (pathname-host #.(or *compile-file-truename*
					    *load-truename*))
		 :directory (pathname-directory #.(or *compile-file-truename*
						      *load-truename*))))
(defparameter dir-resource (merge-pathnames #P"res/" ourdir))
(defparameter dir-shader (merge-pathnames #P"shaders/" dir-resource))
(defparameter dir-mc-assets (merge-pathnames "moreshit/" dir-resource))
(defun shader-path (name)
  (merge-pathnames name dir-shader))
(defun img-path (name)
  (merge-pathnames name dir-mc-assets))

(defparameter *camera* nil) ;;global camera
(defparameter *fog-ratio*  0.75)

(defparameter *pos-previous* (cg-matrix:vec 0.0 0.0 0.0))
(defparameter *pos-current* (cg-matrix:vec 0.0 0.0 0.0))

;;(defparameter *last-yaw* 0.0)
;;(defparameter *last-pitch* 0.0)
(defun render (partial-time)
  (declare (optimize (safety 3) (debug 3)))
  (setf (camera-aspect-ratio *camera*) (/ window:*width* window:*height* 1.0))

  (set-render-cam-pos *camera* partial-time)
  (update-matrices *camera*)
  (let* ((blockshader (aplayground::get-stuff
		       :blockshader
		       aplayground::*stuff*
		       aplayground::*backup*))
	 (blockshader-uniforms *blockshader-uniforms*)
	 (fogcolor (aplayground::getuniform blockshader-uniforms :fog-color))
	 (aratio (aplayground::getuniform blockshader-uniforms :aratio))
	 (foglet (aplayground::getuniform blockshader-uniforms :foglet))
	 (pmv (aplayground::getuniform blockshader-uniforms :pmv))
	 (cam-pos (aplayground::getuniform blockshader-uniforms :cam-pos)))
    (gl:use-program blockshader)
    (let ((time daytime))
      (let ((avector (load-time-value (cg-matrix:vec 0.0 0.0 0.0))))
	(flet ((fractionalize (x)
		 (clamp x 0.0 1.0)))
	  (let ((x (fractionalize (* time #.(nth 2 '(or 1.0 0.0 0.68)))))
		(y (fractionalize (* time #.(nth 2 '(or (/ 139 255.0) 0.0 0.8)))))
		(z (fractionalize (* time #.(nth 1 '(or 0.0 (/ 139 255.0) (/ 205 255.0) 1.0))))))
	    (gl:clear-color x y z 1.0)
	    (setf (aref avector 0) x
		  (aref avector 1) y
		  (aref avector 2) z)
	    (gl:uniformfv fogcolor avector)
	    (gl:uniformfv cam-pos (camera-vec-position *camera*))
	    (gl:uniformf foglet (/ -1.0 (camera-frustum-far *camera*) *fog-ratio*))
	    (gl:uniformf aratio (/ 1.0 *fog-ratio*))))))
    (gl:disable :blend)
    (gl:uniform-matrix-4fv
     pmv
     (camera-matrix-projection-view-player *camera*)
     nil))
  ;;;static geometry with no translation whatsoever
 ;; (sandbox::bind-default-framebuffer)
  (draw-chunk-meshes)
  (designatemeshing))

(defparameter *velocity* (cg-matrix:vec 0.0 0.0 0.0))
(defparameter *orientation* (make-array 6 :element-type 'single-float
					:initial-contents
					'(0.0 0.0 0.0 0.0 0.0 0.0)))

(defun set-render-cam-pos (camera partial)
  (let ((vec (camera-vec-position camera))
	(cev (camera-vec-noitisop camera))
	(prev *pos-previous*)
	(curr *pos-current*))

    (setf (aref prev 0) *xpos-old*)
    (setf (aref prev 1) *ypos-old*)
    (setf (aref prev 2) *zpos-old*)
    
    (setf (aref curr 0) *xpos*)
    (setf (aref curr 1) *ypos*)
    (setf (aref curr 2) *zpos*)

    (cg-matrix:%vec-lerp vec prev curr partial)
    (cg-matrix:%vec* cev vec -1.0)
    
    (unit-pitch-yaw (camera-vec-forward *camera*)
		    (coerce *pitch* 'single-float)
		    (coerce *yaw* 'single-float))

 ;;   (princ " ")
  ;;  (princ *pitch*)
  ;;  (alc:make-context-current fuck::*alc-context*)
   
    (al:listener :position vec)
    (let ((curr *velocity*))
      (setf (aref curr 0) *xvel*)
      (setf (aref curr 1) *yvel*)
      (setf (aref curr 2) *zvel*)
      (al:listener :velocity curr))
    (let ((curr *orientation*)
	  (other (camera-vec-forward *camera*))
	  (other2 (camera-vec-up *camera*)))
      (setf (aref curr 0) (- (aref other 0)))
      (setf (aref curr 1) (- (aref other 1)))
      (setf (aref curr 2) (- (aref other 2)))
      (setf (aref curr 3) (aref other2 0))
      (setf (aref curr 4) (aref other2 1))
      (setf (aref curr 5) (aref other2 2))
      (al:listener :orientation curr))
    
    (setf (camera-fov *camera*) defaultfov)
    ))

(defun draw-chunk-meshes ()
  (gl:enable :depth-test)  
  (gl:depth-func :less)
  (gl:enable :cull-face)
  (gl:cull-face :back
		)
  (gl:bind-texture
   :texture-2d
   (aplayground::get-stuff
    :terrain
    aplayground::*stuff*
    aplayground::*backup*))
  (let ((call-list
	 (aplayground::get-stuff
	  :world
	  aplayground::*stuff*
	  aplayground::*backup*
	  )))
    (when call-list
      (gl:call-list call-list))))
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

(defun update-world-vao ()
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
    (dolist (x (sort list #'< :key (lambda (x)
				(multiple-value-bind (i j k) (world:unhashfunc x)
				  (distance-to-player (- i 8)
						      
						      (- k 8)
						      (- j 8))))))
      (dirty-push x))))

(defun distance-to-player (x y z)
  (let ((dx (- *xpos* x))
	(dy (- *ypos* y))
	(dz (- *zpos* z)))
    (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))

(defun glinnit ()
  (setf *camera* (make-camera))
  (setf mesher-thread nil)

  (aplayground::bornfnc :world #'draw-world)
  (progn
    (aplayground::bornfnc
     :terrain-png
     (lambda ()
       (aplayground::flip-image
	(aplayground::load-png 
	 (img-path #P"terrain.png")))))
    (aplayground::bornfnc
     :grass-png
     (lambda ()
       (aplayground::load-png 
	(img-path #P"misc/grasscolor.png"))))
    (aplayground::bornfnc
     :terrain
     (lambda ()
       (prog1
	   (lovely-shader-and-texture-uploader:pic-texture
	    (aplayground::get-stuff :terrain-png aplayground::*stuff*
				    aplayground::*backup*)
	    :rgba)
;	 (gl:generate-mipmap :texture-2d)
	 (lovely-shader-and-texture-uploader::apply-tex-params
	  (quote ((:texture-min-filter . :nearest;-mipmap-nearest
				       )
		  (:texture-mag-filter . :nearest)
		  (:texture-wrap-s . :repeat)
		  (:texture-wrap-t . :repeat)))))))
    (aplayground::bornfnc
     :blockshader
     (lambda ()
       (let ((program
	      (lovely-shader-and-texture-uploader:make-shader-program-from-strings
	       (aplayground::get-stuff :bs-vs
				      aplayground::*stuff*
				      aplayground::*backup*)
	       (aplayground::get-stuff :bs-frag
				       aplayground::*stuff*
				       aplayground::*backup*)
	       (quote (("position" . 0)	
		       ("texCoord" . 2)
		       ("darkness" . 8)
		       )))))
	 (let ((table (aplayground::make-eq-hash)))
	   (setf *blockshader-uniforms* table)
	   (aplayground::cache-program-uniforms
	    program
	    table
	    (quote ((:pmv . "projectionmodelview")
		    (:fog-color . "fogcolor")
		    (:aratio . "aratio")
		    (:cam-pos . "cameraPos")
		    (:foglet . "foglet")
		    ))))
	 program)))
    (aplayground::bornfnc
     :bs-vs
     (lambda () (aplayground::file-string (shader-path "blockshader/transforms.vs"))))
    (aplayground::bornfnc`
     :bs-frag
     (lambda () (aplayground::file-string (shader-path "blockshader/basictexcoord.frag"))))))
(defparameter *blockshader-uniforms* nil)

;;matrix multiplication is associative
#+nil(defparameter *temp-matrix* (cg-matrix:identity-matrix))
 ;;;opengl stored matrices the transpose of sb-cga


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
(defparameter *temp-matrix2* (cg-matrix:identity-matrix))
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
