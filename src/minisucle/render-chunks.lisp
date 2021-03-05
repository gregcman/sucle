(defpackage #:render-chunks
  (:use :cl :utility)
  (:export
   :mesh-chunk
   :block-shape
   :draw-dispatch
   :with-texture-translator2)
  (:export
   :side-i
   :side+i
   :side-j
   :side+j
   :side-k
   :side+k))
(in-package :render-chunks)

(defparameter *mesh-etex* nil)
(defparameter *mesh-dark* nil)
(defparameter *mesh-epos* nil)

;;(defgeneric draw-dispatch (obj i j k))
(defun draw-dispatch (type i j k)
  (unless (= type 0)
    (renderstandardblock type i j k)))

(defparameter *getlightfun* (constantly 0))
(defparameter *skygetlightfun* (constantly 0))
(defparameter *getblockfun* (constantly 0))

(defun getlight (x y z)
  (funcall *getlightfun* x y z))
(defun skygetlight (x y z)
  (funcall *skygetlightfun* x y z))
(defun getblock (x y z)
  (funcall *getblockfun* x y z))

(with-unsafe-speed
  (defun mesh-chunk (iter &optional (io 0) (jo 0) (ko 0) (isize 16) (jsize 16) (ksize 16))
    (declare (type voxel-chunks:block-coord io jo ko))
    (let* ((lightfun (lambda (x y z)
		       (let ((thing (voxel-chunks:getobj x y z)))
			 (if (sucle::empty-air-p thing)
			     15
			     0))))
	   (*getlightfun* lightfun)
	   (*skygetlightfun* lightfun)
	   (*getblockfun* 'voxel-chunks:getobj))
      (with-vec (*mesh-epos* *mesh-etex* *mesh-dark*) (iter)
	;;(draw-dispatch (voxel-chunks:getobj io jo ko) io jo ko)
	;;#+nil
	(dobox ((i io (the voxel-chunks:block-coord (+ isize io)))
		(j jo (the voxel-chunks:block-coord (+ jsize jo)))
		(k ko (the voxel-chunks:block-coord (+ ksize ko))))
	       (draw-dispatch (getblock i j k) i j k))))))

(eval-always
  ;;;;total faces touched by a light of distance n
  (defun manhattan-perimeter (n)
    (let ((n (+ n 1)))
      (+ (* n n
	    4)
	 2)))

  (defun light-gen-aux-fun (x &optional (max 15))
    (log
     (/
      (/ 1 (manhattan-perimeter (- max x)))
      (/ 1 (manhattan-perimeter max)))))
  #+nil
  (defun gamma-correct (x &optional (gamma-value 2.33))
    (expt x gamma-value))
  
  (defun light-gen (x &optional (max 15))
    ;;Ripped from minecraft beta WorldProvider.java
    #+nil
    (let* ((f 0.05)
	   (i x)
	   (f1 (- 1.0 (/ i 15.0))))
      (+ (* (/ (- 1.0 f1)
	       (+ (* f1 3.0) 1.0))
	    (- 1.0 f))
	 f))
    
    ;;#+nil
    (let ((umm (/ 1.0 (light-gen-aux-fun max max))))
      (*
       (light-gen-aux-fun x max)
       umm))
    
    #+nil
    (gamma-correction:gamma-correct
     #+nil
     (expt 0.8 (- 15 x))
     (let ((a (/ x 15)))
       (* a a))
     1.0)
    )

  (defparameter *light-index-table*
    (let ((foo-array (make-array 16 :element-type 'single-float)))
      (dotimes (x 16)
	(setf (aref foo-array x)
	      (floatify (light-gen x))))
      foo-array)))

(declaim (inline lightfunc))
(defun lightfunc (light)
  (aref (etouq *light-index-table*) light))

(defmacro texface2 (u0 u1 v0 v1 &optional (start 1) (clockwise-winding nil))
  ((lambda (&rest forms)
     (cons (quote progn)
	   (apply (function nconc) forms)))
   ((lambda (value form)
      (mapcar (lambda (x)
		(list value x))
	      form))
    'etex
    (axis-aligned-quads:duaq
     start
     clockwise-winding
     (list u0 u1 v0 v1)))))

#+nil
(defun dark-fun (darkness b0 b1 b2 b3 s0 s1 s2 s3)
  (let ((time *daytime*))
    (* darkness
       0.25
       (+ (max b0 (* time s0))
	  (max b1 (* time s1))
	  (max b2 (* time s2))
	  (max b3 (* time s3))))))

(defmacro squareface (((x0 y0 z0)
		       (x1 y1 z1)
		       (x2 y2 z2)
		       (x3 y3 z3))
		      color	      
		      (i0 j0 k0)		      
		      (i1 j1 k1)		      
		      (i2 j2 k2)		      
		      (i3 j3 k3))
  `(flet ((add-edge (i0 j0 k0)
	    (let ((xpos (+ i i0))
		  (ypos (+ j j0))
		  (zpos (+ k k0)))
	      (declare (type voxel-chunks:block-coord xpos ypos zpos))
	      ,(with-gensyms (actual-color)
		 (flet ((%edge-aux (getfunc q0 q1 q2 q3 &rest body)
			  `(flet ((value (i j k)
				    (let ((xd (+ i xpos))
					  (yd (+ j ypos))
					  (zd (+ k zpos)))
				      (declare (type voxel-chunks:block-coord xd yd zd))
				      (* ,actual-color (lightfunc (,getfunc xd yd zd))))))
			     (let ((,q0 (value ,x0 ,y0 ,z0))
				   (,q1 (value ,x1 ,y1 ,z1))
				   (,q2 (value ,x2 ,y2 ,z2))
				   (,q3 (value ,x3 ,y3 ,z3)))
			       ,@body))))
		   `(let ((,actual-color ,color))
		      (declare (type single-float ,actual-color))
		      ,(%edge-aux
			'getlight 'q0 'q1 'q2 'q3 
			(%edge-aux
			 'skygetlight 'q4 'q5 'q6 'q7
			 ;;Write block light and sky light values
			 `(dark q0 q1 q2 q3 q4 q5 q6 q7)
			 #+nil
			 ;;Write block light and sky light values precomputed into one float,
			 ;;as opposed to calculating light in the vertex shader
			 `(dark (dark-fun ,actual-color q0 q1 q2 q3 q4 q5 q6 q7))))))))))
     (add-edge ,i0 ,j0 ,k0)
     (add-edge ,i1 ,j1 ,k1)
     (add-edge ,i2 ,j2 ,k2)
     (add-edge ,i3 ,j3 ,k3)))

(defmacro posface ((x0 y0 z0) 
		   (x1 y1 z1)		      
		   (x2 y2 z2)		     
		   (x3 y3 z3)) 
  `(flet ((add (x y z)
	    (let ((xp (+ i x))
		  (yp (+ j y))
		  (zp (+ k z)))
	      (declare (type voxel-chunks:block-coord xp yp zp))
	      (epos (floatify xp)
		    (floatify yp)
		    (floatify zp)))))
     (add ,x0 ,y0 ,z0)
     (add ,x1 ,y1 ,z1)
     (add ,x2 ,y2 ,z2)
     (add ,x3 ,y3 ,z3)))

(defmacro face-header (name &body body)
  `(defun ,name (i j k u0 v0 u1 v1)
     (declare (type voxel-chunks:block-coord i j k)
	      (type single-float u0 v0 u1 v1))
     (scratch-buffer:bind-out* ((*mesh-epos* epos)
				(*mesh-etex* etex)
				(*mesh-dark* dark)) 
      ,@body)))

(eval-always
  (defun simple-float-array (&rest args)
    (make-array (length args) :initial-contents args :element-type 'single-float))
  (defparameter *blockface-color*  
    (simple-float-array 0.6 0.6 0.5 1.0 0.8 0.8)
    ;;(simple-float-array 0.55 0.95 0.2 1.0 0.45 0.85)
    ;;#+nil
    ;;(simple-float-array 1.0 1.0 1.0 1.0 1.0 1.0)
    #+nil
    (let ((side (expt 0.5 2.33)))
      (simple-float-array side side 0.0 1.0 side side))
    ))

(etouq
  (let
    ((light-edge-i
	'((0 1 1)
	  (0 0 1)
	  (0 0 0)
	  (0 1 0)))
     (light-edge-j   
       '((1 0 1)
	 (0 0 1)
	 (0 0 0)
	 (1 0 0)))
     (light-edge-k
       '((1 1 0)
	 (0 1 0)
	 (0 0 0)
	 (1 0 0))))
    `(#+(not (or sbcl ecl))
	progn ;;ccl
	#+(or sbcl ecl)
	with-unsafe-speed
	(face-header side-i  
	  (posface (0 0 0)
		   (0 0 1)
		   (0 1 1)
		   (0 1 0))
	  (texface2 u0 u1 v0 v1 3 nil)
	  (squareface ,light-edge-i
		      (etouq (aref *blockface-color* 0))
		      (-1 -1 -1)
		      (-1 -1 00)
		      (-1 00 00)
		      (-1 00 -1)))
	(face-header side+i  
	  (posface (1 0 0)
		   (1 1 0)
		   (1 1 1)
		   (1 0 1))
	  (texface2 u0 u1 v0 v1 4 nil)
	  (squareface ,light-edge-i
		      (etouq (aref *blockface-color* 1))
		      (1 -1 -1)
		      (1 00 -1)
		      (1 00 00)
		      (1 -1 00)))
	(face-header side-j
	  (posface (0 0 0)
		   (1 0 0)
		   (1 0 1)
		   (0 0 1))
	  (texface2 u0 u1 v0 v1 3 nil) 
	  (squareface ,light-edge-j
		      (etouq (aref *blockface-color* 2))
		      (-1 -1 -1)		   
		      (00 -1 -1)		  
		      (00 -1 00)		   
		      (-1 -1 00)))
	(face-header side+j 
	  (posface (0 1 0)
		   (0 1 1)
		   (1 1 1)
		   (1 1 0))
	  (texface2 u0 u1 v0 v1 3 nil)
	  (squareface ,light-edge-j
		      (etouq (aref *blockface-color* 3))
		      (-1 1 -1)
		      (-1 1 00)
		      (00 1 00)
		      (00 1 -1)))
	(face-header side-k 
	  (posface (0 0 0)
		   (0 1 0)
		   (1 1 0)
		   (1 0 0))
	  (texface2 u0 u1 v0 v1 4 nil)
	  (squareface ,light-edge-k
		      (etouq (aref *blockface-color* 4))
		      (-1 -1 -1)
		      (-1 00 -1)
		      (00 00 -1)
		      (00 -1 -1)))
	(face-header side+k
	  (posface (0 0 1)
		   (1 0 1)
		   (1 1 1)
		   (0 1 1))
	  (texface2 u0 u1 v0 v1 3 nil)
	  (squareface ,light-edge-k
		      (etouq (aref *blockface-color* 5))
		      (-1 -1 1)
		      (00 -1 1)    
		      (00 00 1)    
		      (-1 00 1))))))

(defmacro with-texture-translator2 ((u0 u1 v0 v1) num-form &body body)
  (let ((id (gensym)))
    `(let ((,id (* 4 ,num-form)))
       ,(apply #'with-vec-params `((,id ,u0 ,v0 ,u1 ,v1)) `(,*16x16-tilemap*)
	       body))))

(eval-always
  (defparameter *16x16-tilemap* (rectangular-tilemap:regular-enumeration 16 16)))

(defmacro flipuv (&optional (i 'i) (j 'j) (k 'k) (u1 'u1) (u0 'u0) (v1 'v1) (v0 'v0))
  (utility:with-gensyms (u v)
    `(locally ;;(declare (inline block-hash))
       (multiple-value-bind (,u ,v) (values t nil) ;;(block-hash ,i ,j ,k)
	 (when ,u
	   (rotatef ,u1 ,u0))
	 (when ,v
	   (rotatef ,v1 ,v0))))))

(defun show-sidep (blockid other-blockid)
  (or (sucle::empty-air-p other-blockid)
      (and (not (eql blockid other-blockid))
	   ;;(not (data other-blockid :opaque))
	   )))

(defun renderstandardblock (id i j k)
  ;;FIXME: dummy texture
  (let ((texid 2 ;;(data id :texture)
	  ))
    (with-texture-translator2 (u0 u1 v0 v1) texid
      (flipuv)
      (let ((adj-id (getblock i (1- j) k)))
	(when (show-sidep id adj-id)
	  (side-j i j k u0 v0 u1 v1)))
      (let ((adj-id (getblock i (1+ j) k)))
	(when (show-sidep id adj-id)
	  (side+j i j k u0 v0 u1 v1)))
      (let ((adj-id (getblock (1- i) j k)))
	(when (show-sidep id adj-id)
	  (side-i i j k u0 v0 u1 v1)))
      (let ((adj-id (getblock (1+ i) j k)))
	(when (show-sidep id adj-id)
	  (side+i i j k u0 v0 u1 v1)))    
      (let ((adj-id (getblock i j (1- k))))
	(when (show-sidep id adj-id)
	  (side-k i j k u0 v0 u1 v1)))
      (let ((adj-id (getblock i j (1+ k))))
	(when (show-sidep id adj-id)
	  (side+k i j k u0 v0 u1 v1))))))

;;;;;;

(defun use-chunk-shader (&key (camera *camera*)
			   (sky-color (list (random 1.0) (random 1.0) (random 1.0)))
			   (fog-ratio 0.01)
			   (time-of-day (random 1.0))
			   (chunk-radius (error "chunk-radius not supplied")))
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
			 (* vocs:+size+ chunk-radius)
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
					;#+nil
  (let ((call-list (deflazy:getfnc 'chunk)))
    (glhelp:slow-draw call-list))
  #+nil
  (multiple-value-bind (shown hidden overridden) (draw-world)
    (declare (ignorable shown hidden overridden))
    ;;Wow, so occlusion queries reduce the amount of chunks shown by 10 to 25 times? who knew?
    #+nil
    (when (not (zerop overridden))
      (print overridden))
    ;;(print (/ hidden (+ 0.0 hidden shown)))
    ;;#+nil
    #+nil
    (let ((total (hash-table-count *g/chunk-call-list*)))
      (unless (zerop total)
	(format t "~%~s" (* 100.0 (/ shown total 1.0)))))))
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
      (modify-greens 5 12 :color
		     (color)
		    
		     ;(foliage-color 255 0)
		     :terrain terrain)
      (modify-greens 0 15 :color
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
			  (terrain (error "no image")))
    (let* (;;Assume texture is a square grid of squares
	   (size (round (sqrt (/ (array-total-size terrain) 4))))
	   (cell-size (/ size 16)))
 
      (setf xpos (* xpos cell-size)
	    ypos (* ypos cell-size))
      (dobox ((x xpos (+ cell-size xpos))
	      (y ypos (+ cell-size ypos)))
	     (let ((vecinto (getapixel (- (- size 1) y)
				       x terrain)))
	       (map-into vecinto (lambda (a b)
				   (truncate (* a b) 256))
			 vecinto
			 color))))))

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


(defun foliage-color (a b)
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
  #+nil
  (img:load
   (sucle-temp:path #P"res/terrain.png"))
  (img:load
   (sucle-temp:path #P"res/terrain1-8.png")))

(deflazy:deflazy modified-terrain-png (terrain-png)
  (color-grasses
   (alexandria:copy-array terrain-png)))

(glhelp:deflazy-gl terrain (modified-terrain-png)
  (glhelp:wrap-opengl-texture
   (glhelp:create-opengl-texture-from-data modified-terrain-png)))

(glhelp:deflazy-gl blockshader ()
  (let ((glhelp::*glsl-version* sucle::*shader-version*))
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
     `(("position" ,sucle::*position-attr*) 
       ("texcoord" ,sucle::*texcoord-attr*)
       ("blocklight" 4)
       ("skylight" 5))
     '((:pmv "projection_model_view")
       (:fogcolor "fogcolor")
       (:foglet "foglet")
       (:aratio "aratio")
       (:camera-pos "camera_pos")
       (:sampler "sampler")
       (:time "time")))))


;;;;

(defun attrib-buffer-iterators ()
  (map-into (make-array 3 :element-type t :initial-element nil)
	    (function scratch-buffer:my-iterator)))

(glhelp:deflazy-gl chunk ()
  (render-chunk))

(defun render-chunk ()
  (let ((iterators (attrib-buffer-iterators)))
    (render-chunks:mesh-chunk iterators 0 0 0 32 32 32)
    (update-chunk-mesh iterators)))

(defun update-chunk-mesh (iter)
  (utility:with-vec (a b c) (iter)
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
		    ((sucle::*texcoord-attr* (uv) (uv))
		     (4 (dark) (dark) (dark) (dark))
		     (5 (dark) (dark) (dark) (dark))
		     ;;[FIXME]zero always comes last for display lists?
		     ;;have to figure this out manually?
		     (sucle::*position-attr* (xyz) (xyz) (xyz) 1.0)))))))
	  (return-from update-chunk-mesh (values display-list)))))))
