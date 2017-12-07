(in-package :fuck)

(defparameter *thread* nil)
(defun main ()
  (when (or (eq nil *thread*)
	    (not (bordeaux-threads:thread-alive-p *thread*)))
    (setf *thread*
	  (bordeaux-threads:make-thread
	   (just-main)))))

(defun just-main ()
  (let ((stdo *standard-output*))
    (lambda ()
      (progv (cons '*standard-output* *arguments*)
	  (cons stdo *argument-values*)
	(window::wrapper #'init)))))

(defparameter *arguments* '(window::*iresizable*
			    window::*iwidth*
			    window::*iheight*))
(defparameter *argument-values* (list nil 720 480))

(progn
  (defparameter *trampoline* (lambda (exit-token) (throw exit-token (values))))
  (defun call-trampoline ()
    (let ((value (gensym)))
      (catch value
	(loop
	   (trampoline-bounce value *trampoline*))))))

(defun trampoline-bounce (exit-sym fun)
  (when window:*status*
    (throw exit-sym (values)))
  (window:poll)
  (window::update-control-state *control-state*)
  (funcall fun exit-sym)
  (window:update-display))

(progn
  (defun namexpr (hash name func)
    (setf (gethash name hash) func))
  (defun get-stuff (name stuff otherwise secondary)
    (multiple-value-bind (val exists-p) (gethash name stuff)
      (if exists-p
	  val
	  (let ((genfunc (gethash name otherwise)))
	    (cond ((functionp genfunc)
		   (multiple-value-bind (value secondary-value) (funcall genfunc)
		     (setf (gethash name stuff) value)
		     (setf (gethash name secondary) secondary-value)
		     value))
		  (t (error "backup function not a function: ~a" genfunc))))))))

(progn
  (defparameter *backup* (make-hash-table :test 'eq))
  (defparameter *stuff* (make-hash-table :test 'eq))
  (defparameter *secondary* (make-hash-table :test 'eq))
  (defun bornfnc (name func)
    (namexpr *backup* name func))
  (defun getfnc (name)
    (get-stuff name *stuff* *backup* *secondary*))
  (defun map-stuffs (fun)
    ;;;fun = (key value secondary-value)
    (maphash (lambda (k v)
	       (funcall fun k v (gethash k *secondary*)))
	     *stuff*))
  (defun remove-stuff (k)
    (remhash k *secondary*)
    (remhash k *stuff*)))
(defun scrub-old-gl ()
  (map-stuffs
   (lambda (k v secondary-value)
     (declare (ignorable v))
     (when (eq secondary-value :opengl)
       (remove-stuff k)))))

(progn
  (defclass render-area ()
    ((x :accessor render-area-x
	:initform 0
	:initarg :x)
     (y :accessor render-area-y
	:initform 0
	:initarg :y)
     (width :accessor render-area-width
	    :initform 0
	    :initarg :width)
     (height :accessor render-area-height
	     :initform 0
	     :initarg :height)))
  (defun set-render-area (render-area)
    (with-slots (x y width height) render-area
      (%set-render-area x y width height)))
  (defun %set-render-area (x y width height)
    (gl:viewport x y width height)
    (gl:scissor x y width height)))

;;;time in microseconds
(defun microseconds ()
  (multiple-value-bind (s m) (sb-ext:get-time-of-day)
    (+ (* (expt 10 6) (- s 1506020000)) m)))
(defun tick (ticker fun &optional (time (microseconds)))
  (tickr:tick-update ticker time)
  (tickr:tick-physics ticker fun)
  (float (/ (tickr:ticker-accumulator ticker)
	    (tickr:ticker-dt ticker))))


(defparameter *control-state* (window::make-control-state
			       :curr window::*input-state*))
(defparameter *camera* (camat:make-camera
			:frustum-far (* 256.0)
			:frustum-near (/ 1.0 8.0)))
(defparameter *render-area* (make-instance 'render-area))

(defparameter *ticker* nil)

(defun init ()
  (setf %gl:*gl-get-proc-address* (window:get-proc-address))
  (window:set-vsync t)
  (gl:enable :scissor-test)
  (scrub-old-gl)
  (setf *ticker*
	(tickr:make-ticker
	 :dt (floor 1000000 60)
	 :current-time (microseconds)))
  (dolist (x *pre-trampoline-hooks*) (funcall x))
  (call-trampoline))
(defparameter *pre-trampoline-hooks* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					
(progn
  (declaim (ftype (function (single-float) single-float)
		  translator))
  (fuktard::with-unsafe-speed
    (defun translator (x)
      (let* ((a (* x 0.6))
	     (b (+ 0.2 a))
	     (c (* b b b))
	     (d (* 8.0 0.15 (/ (coerce pi 'single-float) 180.0)))
	     (e (* d c)))
	(declare (type single-float a b c d e))
	e))))

(defparameter *mouse-multiplier* (translator 0.5))

(defun delta2 ()
  (let ((mult *mouse-multiplier*))
    (multiple-value-bind (dx dy) (delta)
      (let ((dyaw (- (* dx mult)))
	    (dpitch (* dy mult)))
	(values dyaw dpitch)))))
(defun delta ()
  (let ((mouse-data (load-time-value (cons 0 0))))
    (multiple-value-bind (newx newy) (window:get-mouse-position)
      (multiple-value-prog1 (values
			     (- newx (car mouse-data))
			     (- newy (cdr mouse-data)))
	(setf (car mouse-data) newx
	      (cdr mouse-data) newy)))))

(defparameter mousecapturestate nil)
(defun remove-spurious-mouse-input ()
  (if (window:mice-locked-p)
      (case mousecapturestate
	((nil)
	 (delta) ;;toss spurious mouse movement 
	 (setf mousecapturestate :justcaptured))
	(:justcaptured (setq mousecapturestate t))
	((t)))
      (when mousecapturestate
	(setq mousecapturestate nil))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wasd-mover (w? a? s? d?)
  (let ((x 0)
	(y 0))
    (when w? (decf x))
    (when a? (decf y))
    (when s? (incf x))
    (when d? (incf y))
    (if (and (zerop x)
	     (zerop y))
	nil
	(atan y x))))

(defun num-key-jp (control-state)
  (etouq
   (cons
    'cond
    (mapcar
     (lambda (n)
       `((window::skey-j-p
	  (window::keyval ,(intern (write-to-string n) :keyword))
	  control-state) ,n))
     '(0 1 2 3 4 5 6 7 8 9)))))

(defparameter *sandbox-on* t)
(setf sandbox::*some-saves* #P"/home/imac/Documents/lispysaves/saves/sandbox-saves/")
(defun sandbox-init ()
  (clrhash sandbox::*g/chunk-call-list*))
(push 'sandbox-init *pre-trampoline-hooks*)

(defparameter *ents* (map-into (make-array 10) #'sandbox::gentity))
(defparameter *ent* (aref *ents* 1))

(defparameter *paused* nil)
(defun physss ()
  (let* ((player-farticle (sandbox::entity-particle *ent*))
	 (pos (sandbox::farticle-position player-farticle))
	 (control-state *control-state*))
    (sandbox::meta-controls control-state
			    *ent*)
    (when (window::skey-j-p (window::keyval :x) control-state)
      (toggle *paused*))
    (let ((num (num-key-jp *control-state*)))
      (when num
	(setf *ent* (aref *ents* num))))
    #+nil
    (when (window::skey-j-p (window::keyval :j) control-state)
       (atest::wowz))
    (unless *paused*
      (setf (sandbox::entity-hips *ent*)
	    (wasd-mover
	     (window::skey-p (window::keyval :e) control-state)
	     (window::skey-p (window::keyval :s) control-state)
	     (window::skey-p (window::keyval :d) control-state)
	     (window::skey-p (window::keyval :f) control-state)))
      (sandbox::physentity *ent*))
    (let ((backwardsbug (load-time-value (cg-matrix:vec 0.0 0.0 0.0))))
      (cg-matrix:%vec* backwardsbug (camat:camera-vec-forward *camera*) -1.0)
      (sandbox::use-fists control-state backwardsbug
			  pos))))

(defun farticle-to-camera (farticle camera fraction)
  (let ((curr (sandbox::farticle-position farticle))
	(prev (sandbox::farticle-position-old farticle)))
    (let ((vec (camat:camera-vec-position camera))
	  (cev (camat:camera-vec-noitisop camera)))
      (cg-matrix:%vec-lerp vec prev curr fraction)
      (cg-matrix:%vec* cev vec -1.0))))
(defun entity-to-camera (entity camera fraction)
  (sandbox::necktovec (sandbox::entity-neck entity)
		      (camat:camera-vec-forward camera))	  
  (farticle-to-camera (sandbox::entity-particle entity)
		      camera
		      fraction))

(defun change-entity-neck (entity yaw pitch)
  (let ((neck (sandbox::entity-neck entity)))
    (sandbox::look-around neck yaw pitch)))

(defparameter *fov*
  ((lambda (deg)
     (* deg (coerce (/ pi 180.0) 'single-float)))
   70))

(setf *trampoline* 'atick)
(defun atick (session)
  (declare (ignorable session))
  ((lambda (width height)
      (let ((camera *camera*))
	(setf (camat:camera-aspect-ratio camera)
	      (/ (coerce width 'single-float)
		 (coerce height 'single-float))))
      (let ((render-area *render-area*))
	(setf (render-area-width render-area) (- width 40)
	      (render-area-height render-area) (- height 40))))
   window::*width* window::*height*)
  (set-render-area *render-area*)
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear
   :color-buffer-bit
   :depth-buffer-bit
   )
  (remove-spurious-mouse-input)   
  (setf (camat:camera-fov *camera*) *fov*)
  (when *sandbox-on*
    (when (window:mice-locked-p)
      (multiple-value-call #'change-entity-neck *ent* (delta2)))
    (entity-to-camera *ent* *camera*
		      (tick *ticker* #'physss))
    (camat:update-matrices *camera*)
    (camera-shader *camera*))
  )

(defun camera-shader (camera)
  (declare (optimize (safety 3) (debug 3)))
  (gl:use-program (getfnc 'blockshader))
  
  (glhelp:with-uniforms uniform (getfnc 'blockshader-uniforms)
    (gl:uniform-matrix-4fv
     (uniform :pmv)
     (camat:camera-matrix-projection-view-player camera)
     nil))
  (gl:bind-texture :texture-2d (funcall #'getfnc 'terrain))
  (gl:enable :depth-test)  
  (gl:depth-func :less)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (sandbox::draw-world)
  (sandbox::designatemeshing))

(progn
  (defun color-grasses (terrain color)
    (modify-greens 64 192 :color color :terrain terrain)
    (modify-greens 80 192 :color color :terrain terrain)
    (modify-greens 0 240 :color color :terrain terrain))
  (defun getapixel (h w image)
    (destructuring-bind (height width c) (array-dimensions image)
      (declare (ignore height))
      (make-array 4 :element-type (array-element-type image)
		  :displaced-to image
		  :displaced-index-offset (* c (+ w (* h width))))))

  #+nil
  (#(1742848/8775 2673664/8775 1079296/8775 255)
    (getapixel 0 0 grass-tint)
    (getapixel 255 255 grass-tint))
;;minecraft fog color sometimes (0.68 0.8 1.0)
  ;;  (progno #(113 174 70 255)  #(198 304 122 255))
;;;grass is 0 240
;;;leaves is [64 80] 192
  (defun modify-greens (xpos ypos
			&key
			  (color #(0 0 0 0))
			  (terrain (error "no image")))
    (dobox ((x xpos (+ 16 xpos)) (y ypos (+ 16 ypos)))
	   ((lambda (vecinto other)
	      (map-into vecinto (lambda (a b) (truncate (* a b) 256)) vecinto other))
	    (getapixel y x terrain) color))))

;;;;load a png image from a path

(defun load-png (filename)
  (opticl:read-png-file filename))

(bornfnc
 'terrain-png
 (lambda ()
   (let ((image
	  (flip-image:flip-image
	   (load-png 
	    (sandbox::sub-path #P"terrain.png"))))
	 (tint nil))
     (let ((grass-tint (getfnc 'grass-png)))
       (setf tint (getapixel 255 0 grass-tint)
	     ))
     (color-grasses
      image
      tint)
     image)))
(bornfnc
 'terrain
 (lambda ()
   (multiple-value-prog1
       (values
	(glhelp:pic-texture
	 (getfnc 'terrain-png)
	 :rgba)
	:opengl)
					;	 (gl:generate-mipmap :texture-2d)
     (glhelp:apply-tex-params
      (quote ((:texture-min-filter . :nearest;-mipmap-nearest
				   )
	      (:texture-mag-filter . :nearest)
	      (:texture-wrap-s . :repeat)
	      (:texture-wrap-t . :repeat)))))))
(bornfnc
 'grass-png
 (lambda ()
   (load-png 
    (sandbox::sub-path #P"grasscolor.png"))))
(progn
  (bornfnc
   'blockshader-uniforms
   (lambda ()
     (glhelp:cache-program-uniforms
      (getfnc 'blockshader)
      '((:pmv . "projectionmodelview")))))
  (bornfnc
   'blockshader
   (lambda ()
     (let ((program
	    (glhelp:make-shader-program-from-strings
	     sandbox::*blockshader-vs*
	     sandbox::*blockshader-frag*
	     (quote (("position" . 0)	
		     ("texCoord" . 2)
		     ("darkness" . 8))))))    
       (values program :opengl)))))


