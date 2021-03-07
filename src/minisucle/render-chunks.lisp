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


(defun dark-fun (darkness b0 b1 b2 b3 s0 s1 s2 s3)
  (let ((time 1.0))
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
			 #+nil
			 ;;Write block light and sky light values
			 `(dark q0 q1 q2 q3 q4 q5 q6 q7)
			 `(let ((rgb (dark-fun ,actual-color q0 q1 q2 q3 q4 q5 q6 q7)))
			    (dark rgb rgb rgb))
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
      #+nil
      (and (not (eql blockid other-blockid))
	   (not (data other-blockid :opaque))
	   )))

(defun renderstandardblock (id i j k)
  ;;FIXME: dummy texture
  (let ((texid id ;;2 ;;(data id :texture)
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

(defun use-chunk-shader (&key (camera *camera*))
  ;;set up shader
  (let ((shader (deflazy:getfnc 'blockshader)))
    (glhelp:use-gl-program shader)

    ;;uniform crucial for first person 3d
    (glhelp:with-uniforms uniform shader
      (gl:uniform-matrix-4fv 
       (uniform :pmv)
       (nsb-cga:matrix*
	(camera-matrix:camera-matrix-projection-view-player camera)
	(nsb-cga:translate* 0.0 0.0 0.0)
	)
       nil))

    ;;other cosmetic uniforms
    (glhelp:with-uniforms uniform shader
      (glhelp:set-uniforms-to-textures
       ((uniform :sampler)
	(glhelp:handle (deflazy:getfnc 'terrain)))))))


(deflazy:deflazy terrain-png ()
  #+nil
  (img:load
   (sucle-temp:path #P"res/terrain.png"))
  ;;#+nil
  (img:load
   (sucle-temp:path #P"res/terrain1-8.png")))

(glhelp:deflazy-gl terrain (terrain-png)
  (glhelp:wrap-opengl-texture
   (glhelp:create-opengl-texture-from-data terrain-png)))

(glhelp:deflazy-gl blockshader ()
  (let ((glhelp::*glsl-version* sucle::*shader-version*))
    (glhelp:create-opengl-shader
     "
in vec4 position;
in vec3 color;
in vec2 texcoord;

out vec3 position_out;
out vec3 color_out;  
out vec2 texcoord_out;

uniform mat4 projection_model_view;

void main () {
gl_Position = projection_model_view * position;
color_out = color;
texcoord_out = texcoord;

position_out = position.xyz;
}"
     "
in vec3 position_out;
in vec3 color_out;
in vec2 texcoord_out;
uniform sampler2D sampler;

void main () {
vec4 pixdata = 
//vec4(mod((position_out.xyz + 0.7) * vec3(0.05,0.07,0.09), 1.0),1.0) *
//vec4(1.0);
texture2D(sampler,texcoord_out.xy);

gl_FragColor.rgb = color_out*pixdata.rgb; 
}"
     `(("position" ,sucle::*position-attr*) 
       ("texcoord" ,sucle::*texcoord-attr*)
       ("color" ,sucle::*color-attr*))
     '((:pmv "projection_model_view")
       (:sampler "sampler")))))


;;;;

(defun attrib-buffer-iterators ()
  (map-into (make-array 3 :element-type t :initial-element nil)
	    (function scratch-buffer:my-iterator)))
#+nil
(glhelp:deflazy-gl chunk ()
  (render-chunk2))
#+nil
(defun render-chunk2 ()
  (let ((iterators (attrib-buffer-iterators))
	(n 33))
    (render-chunks:mesh-chunk iterators 0 0 0 n n n)
    (chunk_iter->vao iterators)))

(defun chunk_iter->vao (iter)
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
		     (sucle::*color-attr* (dark) (dark) (dark))
		     ;;[FIXME]zero always comes last for display lists?
		     ;;have to figure this out manually?
		     (sucle::*position-attr* (xyz) (xyz) (xyz) 1.0)))))))
	  (return-from chunk_iter->vao (values display-list)))))))

;;;;************************************************************************;;;;

(defun render-chunk (x y z)
  (let ((iterators (attrib-buffer-iterators))
	(n 16))
    (render-chunks:mesh-chunk iterators x y z n n n)
    (chunk_iter->vao iterators)))

(struct-to-clos:struct->class
 (defstruct chunk_view
   (table (make-hash-table))
   (voxels voxel-chunks::*voxels*)))

(defun create-chunk_view (&optional (voxels voxel-chunks::*voxels*))
  (make-chunk_view :voxels voxels))

(glhelp:deflazy-gl chunk-view ()
		   (make-chunk_view))
(defun mesh-chunks (&optional (view (deflazy:getfnc 'chunk-view)) &aux (table (chunk_view-table view))
								    (microsecond-timeout 16000)
								    (start-time (fps:microseconds))
								    (timeout-end (+ start-time microsecond-timeout)))
  (block out
    (voxel-chunks::do-chunks-in-cache (chunk (voxel-chunks::voxels-main-cache
					      (chunk_view-voxels view)))
      (let ((key (voxel-chunks::chunk-key chunk))
	    (hash (voxel-chunks::chunk-hash chunk))
	    (chunk-last-modified (voxel-chunks::chunk-last-modified chunk)))
	(multiple-value-bind (view existp) (gethash hash table)
	  (when (or (not existp)
		    (not (eql chunk-last-modified
			      (chunk_mesh-last-modified view)))
		    (not (eql key
			      (chunk_mesh-key view)))
		    ;;(not (glhelp:alive-p (chunk_mesh-mesh view)))
		    )
	    ;;Free up the opengl object from previously
	    (when existp
	      (glhelp::gl-delete* (chunk_mesh-mesh view)))
	    (let ((len 16)
		  (x (voxel-chunks::chunk-x chunk))
		  (y (voxel-chunks::chunk-y chunk))
		  (z (voxel-chunks::chunk-z chunk)))
	      (setf (gethash hash table)
		    (make-chunk_mesh
		     ;;FIXME:: mesh can be nil
		     :mesh (render-chunk (* x len) (* y len) (* z len))
		     :key key
		     :last-modified chunk-last-modified))

	      ;;Timeout so it doesn't mesh too many chunks
	      (when (< timeout-end (fps:microseconds))
		(return-from out)))))))))

(defun render-chunks (&optional (view (deflazy:getfnc 'chunk-view)))
  (mesh-chunks view)
  (gl:enable :depth-test)
  (gl:enable :cull-face)
  (gl:disable :blend)
  (gl:polygon-mode :front-and-back :fill)
  ;;render chunks
  (gl:front-face :ccw)
  #+nil
  (let ((call-list (deflazy:getfnc 'chunk)))
    (glhelp:slow-draw call-list))
  ;;(with-unsafe-speed)
  (glhelp:slow-dispatch draw
    (utility:dohash (k v) (chunk_view-table view)
		    (declare (ignorable k))
		    (let ((mesh (chunk_mesh-mesh v)))
		      (when mesh
			(draw mesh))))))

(struct-to-clos:struct->class
 (defstruct chunk_mesh
   mesh
   key ;;should be EQ to the key in the chunk object
   last-modified
   ))
