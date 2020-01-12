(in-package :sucle)
;;;;************************************************************************;;;;
;;;;<RENDER>

(defun draw-to-default-area ()
  ;;draw to default framebuffer
  (glhelp::bind-default-framebuffer)
  ;;setup clipping area
  (glhelp::set-render-area 0 0 window::*width* window::*height*))


(defparameter *sky-color*
  '(
    ;;0.0 0.0 0.0 1.0
    0.68 0.8 1.0 1.0))
(defparameter *sky-color-foo* '(0.0 0.0 0.0 0.0))
(defun the-sky-color ()
  (map-into *sky-color-foo*
	    (lambda (x)
	    (alexandria:clamp (* x sandbox::*daytime*) 0.0 1.0))
	  *sky-color*))
(defun render-sky ()
  (apply 'gl:clear-color (the-sky-color))
  ;;change the sky color according to time
  (gl:depth-func :less)
  (gl:clear-depth 1.0)
  (cond
    ;;optimization to see if drawing a fullscreen quad is faster than a gl:clear
    #+nil
    (nil 
     (let ((shader (getfnc 'sandbox::gl-clear-color-buffer)))
       (glhelp::use-gl-program shader)
       (glhelp::with-uniforms uniform shader 
	 (with-vec (x y z) (*sky-color-foo*)
	   (%gl:uniform-4f (uniform :color) x y z 1.0))))
     (gl:disable :depth-test)
     ;;(gl:disable :cull-face)

     (gl:depth-mask nil)
     (gl:polygon-mode :front-and-back :fill)
     (sandbox::draw-fullscreen-quad)
     (gl:depth-mask t)
     (gl:clear :depth-buffer-bit))
    (t (gl:clear
	:color-buffer-bit
	:depth-buffer-bit
	))))

(defun use-chunk-shader (&optional (camera *camera*))
  ;;set up shader
  (let ((shader (getfnc 'blockshader)))
    (glhelp::use-gl-program shader)

    ;;uniform crucial for first person 3d
    (glhelp:with-uniforms uniform shader
      (gl:uniform-matrix-4fv 
       (uniform :pmv)
       (camera-matrix:camera-matrix-projection-view-player camera)
       nil))

    ;;other cosmetic uniforms
    (glhelp:with-uniforms
	uniform shader
      (destructuring-bind (r g b . rest) *sky-color-foo*
	(declare (ignorable rest))
	(%gl:uniform-3f (uniform :fogcolor) r g b))
      (gl:uniformfv (uniform :camera-pos)
		    (camera-matrix:camera-vec-position camera))
      (%gl:uniform-1f (uniform :foglet)
		      (/ -1.0
			 ;;FIXME::16 assumes chunk is a 16x16x16 cube
			 (* 16 sandbox::*chunk-radius*)
			 #+nil
			 (or 128 (camera-matrix:camera-frustum-far *camera*))
			 *fog-ratio*))
      (%gl:uniform-1f (uniform :aratio)
		      (/ 1.0 *fog-ratio*))
      (%gl:uniform-1f (uniform :time)
		      sandbox::*daytime*)

      (glhelp::set-uniforms-to-textures
       ((uniform :sampler)
	(glhelp::handle (getfnc 'terrain)))))))
(defun render-chunks ()  
  (gl:enable :depth-test)
  (gl:enable :cull-face)
  (gl:disable :blend)
  (gl:polygon-mode :front-and-back :fill)
  ;;render chunks
  (gl:front-face :ccw)
  (sandbox::get-chunks-to-draw)
  ;#+nil
  (multiple-value-bind (shown hidden) (sandbox::draw-world)
    ;;Wow, so occlusion queries reduce the amount of chunks shown by 10 to 25 times? who knew?
    #+nil
    (let ((total
	   (hash-table-count sandbox::*g/chunk-call-list*)
	    #+nil
	    (+ hidden shown)))
      (unless (zerop total)
	(format t "~%~s" (* 100.0 (/ shown total 1.0)))))))

(defun use-occlusion-shader (&optional (camera *camera*))
  (let ((shader (getfnc 'sandbox::occlusion-shader)))
    (glhelp::use-gl-program shader)
    ;;uniform crucial for first person 3d
    (glhelp:with-uniforms uniform shader
      (gl:uniform-matrix-4fv 
       (uniform :pmv)
       (camera-matrix:camera-matrix-projection-view-player camera)
       nil))))
;;FIXME::better way to do this? bring render-occlusion-queries here?
(defun render-chunk-occlusion-queries ()
  (sandbox::render-occlusion-queries))

#+nil
(defun draw-fullscreen-quad ()
  (gl:call-list
   (glhelp::handle (application::getfnc 'fullscreen-quad))))
#+nil
(glhelp::deflazy-gl fullscreen-quad ()
  (make-instance
   'glhelp::gl-list
   :handle
   (glhelp::with-gl-list
     (macrolet ((vvv (darkness u v x y z)
		  `(progn #+nil(%gl:vertex-attrib-1f 8 ,darkness)
			  #+nil
			  (%gl:vertex-attrib-2f 2 ,u ,v)
			  ;;FIXME::when using %gl:vertex-attrib, the 0 attrib marks the
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
  (glhelp::create-opengl-shader
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
	       (values 255 0)
	       #+nil
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
e	(px-xv3 (- px vx3)))
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

(deflazy terrain-png ()
  (image-utility::load-image-from-file
   (sucle-temp:path #P"res/terrain.png")))

(deflazy modified-terrain-png (terrain-png)
  (color-grasses
   (alexandria::copy-array terrain-png)))

(glhelp:deflazy-gl terrain (modified-terrain-png)
  (glhelp::wrap-opengl-texture
   (glhelp::create-opengl-texture-from-data modified-terrain-png)))
(glhelp:deflazy-gl blockshader ()
  (glhelp::create-opengl-shader
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
//distance(camera_pos.xyz, position.xyz);
max(distance(camera_pos.x, position.x), max(distance(camera_pos.z, position.z),distance(camera_pos.y, position.y)));
fogratio_out = clamp(aratio+foglet*distance, 0.0, 1.0);
}"
   "
in vec2 texcoord_out;
in float color_out;
uniform sampler2D sampler;
in float fogratio_out;
uniform vec3 fogcolor;

void main () {
vec4 pixdata = texture2D(sampler,texcoord_out.xy);
vec3 temp = mix(fogcolor, color_out * pixdata.rgb, fogratio_out);
if (pixdata.a == 0.0){discard;}
gl_FragColor.rgb = temp; 
}"
   '(("position" 2) 
     ("texcoord" 8)
     ("blocklight" 1)
     ("skylight" 0))
   '((:pmv "projection_model_view")
     (:fogcolor "fogcolor")
     (:foglet "foglet")
     (:aratio "aratio")
     (:camera-pos "camera_pos")
     (:sampler "sampler")
     (:time "time"))))

;;;


(glhelp:deflazy-gl solidshader ()
  (glhelp::create-opengl-shader
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
   '(("position" 0) 
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
  ;;FIXME::render the fist again
  (when (fister-exists fist)
    (let ((selected-block (fister-selected-block fist)))
      (with-vec (a b c) (selected-block)
	(let ((iterator (scratch-buffer:my-iterator)))
	  (let ((times (sandbox::draw-aabb a b c *selected-block-aabb* iterator)))
	    (declare (type fixnum times)
		     (optimize (speed 3) (safety 0)))
	    ;;mesh-fist-box
	    (let ((box
		   (let ((n 0.06))
		     ;;FIXME::why use this *iterator*?
		     (scratch-buffer:flush-bind-in* ((iterator xyz))		    
		       (glhelp:create-vao-or-display-list-from-specs
			(:quads times)
			((3 n n n)
			 (0 (xyz) (xyz) (xyz))))
		       ))))
	      (glhelp::slow-draw box)
	      (glhelp::slow-delete box)
	      )))
	))))
#+nil
(defun render-chunk-outline ()
  (draw-aabb
   (* 16.0 sandbox::*chunk-coordinate-center-x*)
   (* 16.0 sandbox::*chunk-coordinate-center-y*)
   (* 16.0 sandbox::*chunk-coordinate-center-z*)
   *chunk-aabb*))
  ;;render crosshairs

(defun render-crosshairs ()
  (glhelp:set-render-area
   (- (/ window::*width* 2.0) 1.0)
   (- (/ window::*height* 2.0) 1.0)
   2
   2)
  (gl:clear-color 1.0 1.0 1.0 1.0)
  (gl:clear
   :color-buffer-bit
   ))

(defun use-solidshader (&optional (camera *camera*))
  (let ((shader (application:getfnc 'solidshader)))
    (glhelp::use-gl-program shader)
    ;;uniform crucial for first person 3d
    (glhelp:with-uniforms
	uniform shader
      (gl:uniform-matrix-4fv 
       (uniform :pmv)
       ;;(nsb-cga::identity-matrix)
       
       (camera-matrix:camera-matrix-projection-view-player camera)
       nil))))


;;;;</RENDER>
