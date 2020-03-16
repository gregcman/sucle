(in-package :sucle)
;;;;************************************************************************;;;;
;;;;<RENDER>

(defun draw-to-default-area ()
  ;;draw to default framebuffer
  (glhelp:bind-default-framebuffer)
  ;;setup clipping area
  (glhelp:set-render-area 0 0 window:*width* window:*height*))

(defun render-sky (x y z)
  (gl:clear-color x y z 1.0)
  ;;change the sky color according to time
  (gl:depth-func :less)
  (gl:clear-depth 1.0)
  (cond
    ;;optimization to see if drawing a fullscreen quad is faster than a gl:clear
    #+nil
    (nil 
     (let ((shader (deflazy:getfnc 'gl-clear-color-buffer)))
       (glhelp:use-gl-program shader)
       (glhelp:with-uniforms uniform shader 
	 (with-vec (x y z) (*sky-color-foo*)
	   (%gl:uniform-4f (uniform :color) x y z 1.0))))
     (gl:disable :depth-test)
     ;;(gl:disable :cull-face)

     (gl:depth-mask nil)
     (gl:polygon-mode :front-and-back :fill)
     (draw-fullscreen-quad)
     (gl:depth-mask t)
     (gl:clear :depth-buffer-bit))
    (t (gl:clear
	:color-buffer-bit
	:depth-buffer-bit
	))))

(defun use-chunk-shader (&key (camera *camera*)
			   (sky-color (list (random 1.0) (random 1.0) (random 1.0)))
			   (fog-ratio 0.01)
			   (time-of-day (random 1.0)))
  ;;set up shader
  (let ((shader (deflazy:getfnc 'blockshader)))
    (glhelp:use-gl-program shader)

    ;;uniform crucial for first person 3d
    (glhelp:with-uniforms uniform shader
      (gl:uniform-matrix-4fv 
       (uniform :pmv)
       (camera-matrix:camera-matrix-projection-view-player camera)
       nil))

    ;;other cosmetic uniforms
    (glhelp:with-uniforms
	uniform shader
      (destructuring-bind (r g b &rest rest) sky-color
	(declare (ignorable rest))
	(%gl:uniform-3f (uniform :fogcolor) r g b))
      (gl:uniformfv (uniform :camera-pos)
		    (camera-matrix:camera-vec-position camera))
      (%gl:uniform-1f (uniform :foglet)
		      (/ -1.0
			 ;;[FIXME]16 assumes chunk is a 16x16x16 cube
			 (* 16 world:*chunk-radius*)
			 #+nil
			 (or 128 (camera-matrix:camera-frustum-far *camera*))
			 fog-ratio))
      (%gl:uniform-1f (uniform :aratio)
		      (/ 1.0 fog-ratio))
      (%gl:uniform-1f (uniform :time)
		      time-of-day)

      (glhelp:set-uniforms-to-textures
       ((uniform :sampler)
	(glhelp:handle (deflazy:getfnc 'terrain)))))))
(defun render-chunks ()  
  (gl:enable :depth-test)
  (gl:enable :cull-face)
  (gl:disable :blend)
  (gl:polygon-mode :front-and-back :fill)
  ;;render chunks
  (gl:front-face :ccw)
  (get-chunks-to-draw)
  ;#+nil
  (progn;;multiple-value-bind (shown hidden)
      (draw-world)
    ;;Wow, so occlusion queries reduce the amount of chunks shown by 10 to 25 times? who knew?
    #+nil
    (let ((total
	   (hash-table-count *g/chunk-call-list*)
	    #+nil
	    (+ hidden shown)))
      (unless (zerop total)
	(format t "~%~s" (* 100.0 (/ shown total 1.0)))))))

(defun use-occlusion-shader (&optional (camera *camera*))
  (let ((shader (deflazy:getfnc 'occlusion-shader)))
    (glhelp:use-gl-program shader)
    ;;uniform crucial for first person 3d
    (glhelp:with-uniforms uniform shader
      (gl:uniform-matrix-4fv 
       (uniform :pmv)
       (camera-matrix:camera-matrix-projection-view-player camera)
       nil))))
;;[FIXME]better way to do this? bring render-occlusion-queries here?
(defun render-chunk-occlusion-queries ()
  (render-occlusion-queries))

#+nil
(defun draw-fullscreen-quad ()
  (gl:call-list
   (glhelp:handle (deflazy:getfnc 'fullscreen-quad))))
#+nil
(glhelp:deflazy-gl fullscreen-quad ()
  (make-instance
   'glhelp:gl-list
   :handle
   (glhelp:with-gl-list
     (macrolet ((vvv (darkness u v x y z)
		  `(progn #+nil(%gl:vertex-attrib-1f 8 ,darkness)
			  #+nil
			  (%gl:vertex-attrib-2f 2 ,u ,v)
			  ;;[FIXME]when using %gl:vertex-attrib, the 0 attrib marks the
			  ;;end.
			  (%gl:vertex-attrib-4f 0 ,x ,y ,z 1.0)
			  )))
       (gl:with-primitives :quads
	 (vvv 0.0 w2 h3 1.0 1.0 0.99999994)
	 (vvv 0.0 w2 h2 -1.0 1.0 0.99999994)
	 (vvv 0.0 w1 h2 -1.0 -1.0 0.99999994)
	 (vvv 0.0 w1 h3 1.0 -1.0 0.99999994))))))
#+nil
(glhelp:deflazy-gl gl-clear-color-buffer ()
  (glhelp:create-opengl-shader
   "in vec4 position;

void main () {
gl_Position = position;

}"
   "
uniform vec4 color = vec4(0.6,0.7,0.2,1.0); 
void main () {
gl_FragColor = color;
}"
   '(("position" 0)) 
   '((:color "color"))))


(defun quadratic-formula (a b c)
  (let ((two-a (+ a a)))
    (let ((term2 (/ (sqrt (- (* b b)
			     (* 4 a c)))
		    two-a))
	  (term1 (/ (- b)
		    two-a)))
      (values (+ term1 term2)
	      (- term1 term2)))))

(defun sum-of-first-n-integers (n)
  (/ (* (+ n 1) n)
     2))

(defun reverse-sum-of-first-n-integers (n)
  (quadratic-formula 0.5 0.5 (- n)))

(defun oct-24-2018 ()
  (let ((pick
	 (random
	  (sum-of-first-n-integers 256))
	  ))
    (let ((a (floor
	      (reverse-sum-of-first-n-integers pick))))
      (values a
	      (- pick (sum-of-first-n-integers a))))))

(progn
  (defun color-grasses (terrain)
    (flet ((color ()
	     (multiple-value-call #'foliage-color
	       ;;(values 255 0)
	       ;;#+nil
	       (oct-24-2018)
	       )
	     
	     ;;;does not distribute evenly. it picks a slice, then a height on the slice.
	     ;;;points on small slices have a greater chance of being picked than
	     ;;;points on large slices.
	     #+nil
	     (let ((value (random 256)))
	       (foliage-color value (random (1+ value))))))
      (modify-greens 80 192 :color
		     (color)
		    
		     ;(foliage-color 255 0)
		     :terrain terrain)
      (modify-greens 0 240 :color
		     (color)
		     
		     ;(foliage-color 255 0)
		     :terrain terrain))
    terrain)
  (defun getapixel (h w image)
    (destructuring-bind (height width c) (array-dimensions image)
      (declare (ignore height))
      (make-array 4 :element-type (array-element-type image)
		  :displaced-to image
		  :displaced-index-offset (* c (+ w (* h width))))))

  (defun modify-greens (xpos ypos
			&key
			  (color #(0 0 0 0))
			  (terrain (error "no image"))
			  (height 256)
			  (texheight 16))
    ;;#+nil
    (setf xpos (* 2 xpos)
	  ypos (* 2 ypos)
	  height (* 2 height)
	  texheight (* 2 texheight))
    (dobox ((x xpos (+ texheight xpos))
	    (y ypos (+ texheight ypos)))
	   ((lambda (vecinto other)
	      (map-into vecinto (lambda (a b) (truncate (* a b) height)) vecinto other))
	    (getapixel (- (- height 1) y) x terrain) color))))

(defun barycentric-interpolation (px py vx1 vy1 vx2 vy2 vx3 vy3)
  (let ((denominator (+ (*
			 (- vy2 vy3)
			 (- vx1 vx3))
			(*
			 (- vx3 vx2)
			 (- vy1 vy3))))
	(py-yv3 (- py vy3))
	(px-xv3 (- px vx3)))
    (let* ((w1 (/
		(+
		 (*
		  (- vy2 vy3)
		  px-xv3)
		 (*
		  (- vx3 vx2)
		  py-yv3))
		  denominator))
	   (w2 (/
		(+
		 (*
		  (- vy3 vy1)
		  px-xv3)
		 (*
		  (- vx1 vx3)
		  py-yv3))
		denominator))
	   (w3 (- 1 w1 w2)))
      (values w1 w2 w3))))


(defun  foliage-color (a b)
  (multiple-value-bind (w1 w2 w3)
      (barycentric-interpolation a b 0.0 0.0 255.0 0.0 255.0 255.0)
    (mapcar (lambda (x y z)
	      (+ (* x w1)
		 (* y w2)
		 (* z w3)))
	    '(71.0 205.0 51.0)
	    '(191.0 183.0 85.0)
	    '(128.0 180.0 151.0))))

(deflazy:deflazy terrain-png ()
  (img:load
   (sucle-temp:path #P"res/terrain.png")))

(deflazy:deflazy modified-terrain-png (terrain-png)
  (color-grasses
   (alexandria:copy-array terrain-png)))

(glhelp:deflazy-gl terrain (modified-terrain-png)
  (glhelp:wrap-opengl-texture
   (glhelp:create-opengl-texture-from-data modified-terrain-png)))
(defparameter *position-attr* 0)
(defparameter *texcoord-attr* 2)

(defparameter *shader-version* 120)
(defun test-all-shader-versions ()
  (dolist (x glhelp::*version-data*)
    (setf *shader-version* (second x))
    (deflazy:refresh 'blockshader nil)
    (sleep 0.5)))

(glhelp:deflazy-gl blockshader ()
  (let ((glhelp::*glsl-version* *shader-version*))
    (glhelp:create-opengl-shader
     "
out float color_out;
out vec2 texcoord_out;
out float fogratio_out;

in vec4 position;
in vec2 texcoord;
in vec4 blocklight;
in vec4 skylight;
uniform mat4 projection_model_view;
uniform float time = 0.0;

uniform float foglet;
uniform float aratio;
uniform vec3 camera_pos;

void main () {
gl_Position = projection_model_view * position;
vec4 light = max(skylight*time, blocklight);
color_out = dot(light,vec4(0.25));
texcoord_out = texcoord;

float distance = 
//distance(position.xyz,vec3(0.0));
distance(camera_pos.xyz, position.xyz);
//max(distance(camera_pos.x, position.x), max(distance(camera_pos.z, position.z),distance(camera_pos.y, position.y)));
fogratio_out = clamp(aratio+foglet*distance, 0.0, 1.0);
}"
     "
in vec2 texcoord_out;
in float color_out;
uniform sampler2D sampler;
in float fogratio_out;
uniform vec3 fogcolor;

void main () {
vec4 pixdata = 
//vec4(1.0);
texture2D(sampler,texcoord_out.xy);
vec3 temp = mix(fogcolor, color_out * pixdata.rgb, fogratio_out);
//if (pixdata.a == 0.0){discard;}
gl_FragColor.rgb = temp; 
}"
     `(("position" ,*position-attr*) 
       ("texcoord" ,*texcoord-attr*)
       ("blocklight" 4)
       ("skylight" 5))
     '((:pmv "projection_model_view")
       (:fogcolor "fogcolor")
       (:foglet "foglet")
       (:aratio "aratio")
       (:camera-pos "camera_pos")
       (:sampler "sampler")
       (:time "time")))))

;;;


(glhelp:deflazy-gl solidshader ()
  (glhelp:create-opengl-shader
   "
out vec3 color_out;
in vec4 position;
in vec3 color;
uniform mat4 projection_model_view;

void main () {
gl_Position = projection_model_view * position;
color_out = color;
}"
   "
in vec3 color_out;
void main () {
gl_FragColor.a = 1.0;
gl_FragColor.rgb = color_out;
}"
   `(("position" ,*position-attr*) 
     ("color" 3))
   '((:pmv "projection_model_view"))))
(defparameter *selected-block-aabb*
  (let* ((offset 0.1)
	 (small (- 0.0 offset))
	 (large (+ 1.0 offset)))
    (create-aabb large large large small small small)))

(defun render-fist (&optional (fist *fist*))
  (gl:disable :blend)
  (gl:disable :cull-face)
  (gl:polygon-mode :front-and-back :line)
  (gl:line-width 2)
  ;;[FIXME]render the fist again
  (when (fister-exists fist)
    (let ((selected-block (fister-selected-block fist)))
      (with-vec (a b c) (selected-block)
	(let ((iterator (scratch-buffer:my-iterator)))
	  (let ((times (draw-aabb a b c *selected-block-aabb* iterator)))
	    (declare (type fixnum times)
		     (optimize (speed 3) (safety 0)))
	    ;;mesh-fist-box
	    (let ((box
		   (let ((n 0.06))
		     ;;[FIXME]why use this *iterator*?
		     (scratch-buffer:flush-bind-in* ((iterator xyz))		    
		       (glhelp:create-vao-or-display-list-from-specs
			(:quads times)
			((3 n n n)
			 (*position-attr* (xyz) (xyz) (xyz))))
		       ))))
	      (glhelp:slow-draw box)
	      (glhelp:slow-delete box)
	      )))
	))))
#+nil
(defun render-chunk-outline ()
  (draw-aabb
   (* 16.0 world:*chunk-coordinate-center-x*)
   (* 16.0 world:*chunk-coordinate-center-y*)
   (* 16.0 world:*chunk-coordinate-center-z*)
   *chunk-aabb*))
  ;;render crosshairs

(defun render-crosshairs ()
  (glhelp:set-render-area
   (- (/ window:*width* 2.0) 1.0)
   (- (/ window:*height* 2.0) 1.0)
   2
   2)
  (gl:clear-color 1.0 1.0 1.0 1.0)
  (gl:clear
   :color-buffer-bit
   ))

(defun use-solidshader (&optional (camera *camera*))
  (let ((shader (deflazy:getfnc 'solidshader)))
    (glhelp:use-gl-program shader)
    ;;uniform crucial for first person 3d
    (glhelp:with-uniforms
	uniform shader
      (gl:uniform-matrix-4fv 
       (uniform :pmv)
       ;;(nsb-cga:identity-matrix)
       
       (camera-matrix:camera-matrix-projection-view-player camera)
       nil))))


;;;;</RENDER>
;;;;************************************************************************;;;;
;;;;<CHUNK-RENDERING?>

(progn
  (defparameter *g/chunk-call-list* (make-hash-table :test 'equal))
  (defun get-chunk-display-list (name)
    (gethash name *g/chunk-call-list*))
  (defun set-chunk-display-list (name list-num)
    (setf (gethash name *g/chunk-call-list*) list-num))
  (defun remove-chunk-display-list (name)
    (remhash name *g/chunk-call-list*))
  (defun reset-chunk-display-list ()
    (clrhash *g/chunk-call-list*)))
(defun remove-chunk-model (name)
  ;;[FIXME]this calls opengl. Use a queue instead?
  (multiple-value-bind (value existsp) (get-chunk-display-list name)
    (when existsp
      (destroy-chunk-gl-representation value)
      (remove-chunk-display-list name))))

;;https://vertostudio.com/gamedev/?p=177
(struct-to-clos:struct->class
 (defstruct chunk-gl-representation
   call-list
   occlusion-query
   occluded
   (occlusion-state :init
		    ) ;;:hidden, visible, waiting, :init
   occlusion-box))
(defparameter *occlusion-culling-p* t)
(defun set-chunk-gl-representation-visible (value)
  (setf (chunk-gl-representation-occlusion-state value) :visible)
  (setf (chunk-gl-representation-occluded value) nil))
(defun set-chunk-gl-representation-hidden (value)
  (setf (chunk-gl-representation-occlusion-state value) :hidden)
  (setf (chunk-gl-representation-occluded value) t))
(defun render-occlusion-query (value)
  (let ((query (chunk-gl-representation-occlusion-query value)))
    (symbol-macrolet ((occlusion-state (chunk-gl-representation-occlusion-state value)))
      (when (not (eq occlusion-state :waiting))
	(setf occlusion-state :waiting)
	(gl:begin-query :samples-passed query)
	;;draw occlusion box here, get occlusion information from a box
	(glhelp:slow-draw (chunk-gl-representation-occlusion-box value))
	(gl:end-query :samples-passed)))))
(defparameter *call-lists* (make-array 0 :fill-pointer 0 :adjustable t))
(defun render-occlusion-queries (&optional (vec *call-lists*))
  (when *occlusion-culling-p*
    (gl:color-mask nil nil nil nil)
    (gl:depth-mask nil)
    (loop :for value :across vec :do
       (render-occlusion-query value))
    (gl:color-mask t t t t)
    (gl:depth-mask t)))

(defun create-chunk-gl-representation (display-list occlusion-box)
  (make-chunk-gl-representation
   :call-list display-list
   :occlusion-query (car (gl:gen-queries 1))
   :occlusion-box occlusion-box))
(defun destroy-chunk-gl-representation (chunk-gl-representation)
  (glhelp:slow-delete (chunk-gl-representation-call-list chunk-gl-representation))
  (gl:delete-queries (list (chunk-gl-representation-occlusion-query chunk-gl-representation))))

(defun get-chunks-to-draw ()
  (let ((vec *call-lists*))
    (setf (fill-pointer vec) 0)
    (let* ((foo (+ 1 world:*chunk-radius*)))
      (dohash (key value) *g/chunk-call-list*
	      ;;(declare (ignore key))
	      (when (> (the fixnum foo) (the fixnum (world:blocky-chunk-distance key)))
		(vector-push-extend value vec))))
    vec))
(defun draw-world (&optional (vec *call-lists*) &aux (count-occluded-by-query 0)
						  (count-actually-drawn 0))
  #+nil
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum count-actually-drawn count-occluded-by-query))
  (declare (optimize (debug 3) (safety 3)))
  ;;(let ((a (get-internal-real-time))))
  (loop :for value :across vec :do
     (let ((display-list (chunk-gl-representation-call-list value))
	   (query (chunk-gl-representation-occlusion-query value)))
       (cond ((and *occlusion-culling-p*
		   (not (eq (chunk-gl-representation-occlusion-state value) :init)))
	      (let ((available (gl:get-query-object query :query-result-available)))
		(when available
		  ;;[FIXME]bug in cl-opengl, gl:get-query-object not implemented for GL<3.3
		  (let ((result (gl:get-query-object query :query-result)))		      
		    (case result
		      (0 (set-chunk-gl-representation-hidden value))
		      (otherwise (set-chunk-gl-representation-visible value)))))))
	     (t (set-chunk-gl-representation-visible value)))
       ;;      (gl:call-list (chunk-gl-representation-occlusion-box value))
       
       (cond
	 ((not (chunk-gl-representation-occluded value)) ;;not occluded = visible
	  (incf count-actually-drawn)
	  (let ((query (chunk-gl-representation-occlusion-query value)))
	    (symbol-macrolet ((occlusion-state (chunk-gl-representation-occlusion-state value)))
	      (cond
		((and *occlusion-culling-p*
		      (not (eq occlusion-state :waiting)))
		 ;;get occlusion information from regular chunk drawing
		 (setf occlusion-state :waiting)
		 (gl:begin-query :samples-passed query)
		 ;;draw occlusion box here
		 ;;(gl:call-list (chunk-gl-representation-occlusion-box value))
		 (glhelp:slow-draw display-list)
		 (gl:end-query :samples-passed))
		(t
		 (glhelp:slow-draw display-list)))))
	  ;;(gl:call-list display-list)
	  )
	 (t ;;(print "WHAT?")
	  (incf count-occluded-by-query)
	  ;;(gl:call-list display-list)
	  ))))
  (values
   count-actually-drawn
   count-occluded-by-query)
  ;;(gl:call-lists vec)
  ;;(print (- (get-internal-real-time) a))
  )



(defparameter *finished-mesh-tasks* (lparallel.queue:make-queue))

#+nil
(defun call-with-world-meshing-lparallel (fun)
  (sucle-mp:with-initialize-multiprocessing
    (funcall fun)))

(defun update-world-vao ()
  (world:clean-dirty)
  (reset-meshers)
  (loop :for key :being :the :hash-keys :of *g/chunk-call-list* :do
     (remove-chunk-model key))
  (mapc #'world:dirty-push
	(sort (alexandria:hash-table-keys voxel-chunks:*chunks*) #'< :key
	      'world:unsquared-chunk-distance)))

(defparameter *chunk-query-buffer-size* 0)
(defvar *iterator*)
(defun update-chunk-mesh (coords iter)
  (when coords
    (remove-chunk-model coords)
    (with-vec (a b c) (iter)
      (let ((len (floor (scratch-buffer:iterator-fill-pointer a) 3)))
	(unless (zerop len)
	  (let ((display-list
		 (utility:with-unsafe-speed
		   (scratch-buffer:flush-bind-in*
		       ((a xyz)
			(b uv)
			(c dark))
		     (glhelp:create-vao-or-display-list-from-specs
		      (:quads len)
		      ((*texcoord-attr* (uv) (uv))
		       (4 (dark) (dark) (dark) (dark))
		       (5 (dark) (dark) (dark) (dark))
		       ;;[FIXME]zero always comes last for display lists?
		       ;;have to figure this out manually?
		       (*position-attr* (xyz) (xyz) (xyz) 1.0))))))
		(occlusion-box	 
		 (multiple-value-bind (x y z) (voxel-chunks:unhashfunc coords)
		   (let ((*iterator* (scratch-buffer:my-iterator)))
		     (let ((times
			    (draw-aabb x y z
				       (load-time-value
					(let ((foo *chunk-query-buffer-size*))
					  (aabbcc:make-aabb
					    :minx (- 0.0 foo)
					    :miny (- 0.0 foo)
					    :minz (- 0.0 foo)
					    :maxx (+ (floatify voxel-chunks:*chunk-size-x*) foo)
					    :maxy (+ (floatify voxel-chunks:*chunk-size-y*) foo)
					    :maxz (+ (floatify voxel-chunks:*chunk-size-z*) foo)))))))
			(scratch-buffer:flush-bind-in*
			 ((*iterator* xyz))
			 (glhelp:create-vao-or-display-list-from-specs
			  (:quads times)
			  (;;Query objects don't need the other attributes
			   ;;(*texcoord-attr* 0.06 0.06)
			   ;;(4 0.0 0.0 0.0 0.0)
			   ;;(5 0.0 0.0 0.0 0.0)
			   (*position-attr* (xyz) (xyz) (xyz) 1.0)))))))))
	    (set-chunk-display-list
	     coords
	     (create-chunk-gl-representation display-list occlusion-box))))))))


(defun draw-aabb (x y z aabb &optional (iterator *iterator*))
  (let ((minx (aabbcc:aabb-minx aabb))
	(miny (aabbcc:aabb-miny aabb))
	(minz (aabbcc:aabb-minz aabb))
	(maxx (aabbcc:aabb-maxx aabb))
	(maxy (aabbcc:aabb-maxy aabb))
	(maxz (aabbcc:aabb-maxz aabb)))
      aabb
    (draw-box
     (+ minx x -0) (+  miny y -0) (+  minz z -0)
     (+ maxx x -0) (+  maxy y -0) (+  maxz z -0)
     iterator)))

(defun draw-box (minx miny minz maxx maxy maxz &optional (iterator *iterator*))
  (macrolet ((vvv (x y z)
	       `(progn
		  (fun ,x)
		  (fun ,y)
		  (fun ,z))
	       ))
    (scratch-buffer:bind-out* ((iterator fun))
      (vvv minx maxy minz)
      (vvv maxx maxy minz)
      (vvv maxx maxy maxz)
      (vvv minx maxy maxz)

      ;;j-
      (vvv minx miny minz)
      (vvv minx miny maxz)
      (vvv maxx miny maxz)
      (vvv maxx miny minz)

      ;;k-
      (vvv minx maxy minz)
      (vvv minx miny minz)
      (vvv maxx miny minz)
      (vvv maxx maxy minz)

      ;;k+
      (vvv maxx miny maxz)
      (vvv minx miny maxz)
      (vvv minx maxy maxz)
      (vvv maxx maxy maxz)
      
      ;;i-
      (vvv minx miny minz)
      (vvv minx maxy minz)
      (vvv minx maxy maxz)
      (vvv minx miny maxz)

      ;;i+
      (vvv maxx miny minz)
      (vvv maxx miny maxz)
      (vvv maxx maxy maxz)
      (vvv maxx maxy minz))

    (values (* 6 4))))

(glhelp:deflazy-gl occlusion-shader ()
  (glhelp:create-opengl-shader
   "in vec4 position;

uniform mat4 projection_model_view;
void main () {
gl_Position = projection_model_view * position;

}"
   "
void main () {
gl_FragColor = vec4(1.0);
}"
   `(("position" ,*position-attr*)) 
   '((:pmv "projection_model_view"))))

(defun attrib-buffer-iterators ()
  (map-into (make-array 3 :element-type t :initial-element nil)
	    (function scratch-buffer:my-iterator)))
(defparameter *chunk-render-radius* 6)
(defparameter *total-background-chunk-mesh-jobs* 0
  "track the number of subprocesses which are actively meshing. Increases when meshing,
decreases when finished.")
(defun meshes-pending-for-gl ()
  "How many lisp-side meshes are waiting to be sent to the GPU?"
  (lparallel.queue:queue-count *finished-mesh-tasks*))
(defparameter *max-meshes-pending-for-gl* 4)
(defparameter *max-total-background-chunk-mesh-jobs* 1)
(defparameter *max-gl-meshing-iterations-per-frame* 2)

(defun reset-meshers ()
  (sucle-mp:with-kernel
    (lparallel:kill-tasks 'mesh-chunk)
    #+nil
    (progn
      (lparallel:kill-tasks :chunk-load)
      (lparallel:kill-tasks :chunk-save))
    (setf *total-background-chunk-mesh-jobs* 0)))
;;We limit the amount of chunks that can be sent to the mesh queue
(defun complete-render-tasks (&optional (iteration-count
					 *max-gl-meshing-iterations-per-frame*))
  "Iterate through submitted tasks that require an openGL context in order to complete.
Mostly used to receive chunks meshes from non-gl threads, upon which the chunk 
meshes are converted to display-lists or VBO/VAOs and saved for later in order
to be drawn by the render thread."
  (when (plusp (meshes-pending-for-gl))
    (let ((count 0))
      (block stop-meshing
	(sucle-mp:do-queue-iterator (job-task *finished-mesh-tasks*)
	  (when (> count *max-gl-meshing-iterations-per-frame*)
	    (return-from stop-meshing))
	  (incf count)
	  (let ((value (car (sucle-mp:job-task-return-values (job-task)))))
	    (cond (value
		   (destructuring-bind (type function . args) value
		     ;;[FIXME]document this somewhere?
		     ;;*finshed-mesh-tasks* becoming a generic command buffer?
		     (assert (eq :mesh-chunk type))
		     (apply function args)))
		  (t (print value)))))))))
(defun dispatch-mesher-to-dirty-chunks ()
  "Re-draw, draw, or delete the openGL representation of chunks based 
observed chunk state changes. 
Chunk state changes can be found in `world:*dirty-chunks*`

Note:limits the amount of background jobs and pending lisp objects."
  (flet
      ;;;Limit the amount of background jobs and pending lisp objects.
      ((too-much ()
	 (or
	  ;;So there are not too many background jobs
	  (<= *max-total-background-chunk-mesh-jobs* *total-background-chunk-mesh-jobs*)
	  ;;So memory is not entirely eaten up
	  (<= *max-meshes-pending-for-gl* (meshes-pending-for-gl)))))
    (when (not (too-much))
      (queue:sort-queue
       world:*dirty-chunks*
       (lambda (list)
	 (sort list
	       ;;remove chunks from the queue that are too far away, don't try to mesh them
	       #+nil ;;WRONG!![FIXME]separate world loading code from opengl
	       (delete-if (lambda (x)
			    (>= (blocky-chunk-distance x) *chunk-render-radius*))
			  list)
	       '< :key 'world:unsquared-chunk-distance)))
      (loop :named submit-mesh-tasks
	 :while (not (too-much)) :do
	 (let ((thechunk (world:dirty-pop)))
	   (if thechunk
	       (cond
		 ;;If the chunk exists and is not empty
		 ((and (voxel-chunks:chunk-exists-p thechunk)
		       (not (voxel-chunks:empty-chunk-p
			     (voxel-chunks:get-chunk-at thechunk))))
		  ;;Then submit a job to the mesher 
		  (incf *total-background-chunk-mesh-jobs*)
		  (let ((lparallel:*task-category* 'mesh-chunk))
		    (sucle-mp:submit 
		     (lambda (iter space chunk-pos)
		       (map nil (lambda (x) (scratch-buffer:free-my-iterator-memory x)) iter)
		       (multiple-value-bind (io jo ko) (voxel-chunks:unhashfunc chunk-pos)
			 (mesher:mesh-chunk iter io jo ko)
			 ;;The return value is used as a callback when
			 ;;sent to the *finished-mesh-task*
			 (%list space :mesh-chunk 'update-chunk-mesh chunk-pos iter)))
		     :args (list
			    (attrib-buffer-iterators)
			    (make-list 4)
			    thechunk)
		     :callback (lambda (job-task)
				 (lparallel.queue:push-queue job-task *finished-mesh-tasks*)
				 (decf *total-background-chunk-mesh-jobs*)))))
		 (t (remove-chunk-model thechunk)))
	       (return-from submit-mesh-tasks)))))))
;;;;<CHUNK-RENDERING?>
;;;;************************************************************************;;;;
