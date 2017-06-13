(in-package :sandbox)

(defun make-attrib-buffer-data ()
  (make-array 16 :element-type t :initial-element nil))
(defun fill-with-flhats (array)
  (map-into array #'flhat:make-flhat))
(defun make-iterators (buffer result)
  (map-into result (lambda (x) (flhat:make-flhat-iterator x)) buffer))
(defun tally-buffer (iterator-buffer result)
  (map-into result (lambda (x) (iterator-count x)) iterator-buffer))
(defun reset-attrib-buffer-iterators (fill-data)
  (dotimes (x (array-total-size fill-data))
    (flhat:reset-iterator (aref fill-data x))))
(defun iterator-count (iterator)
  (if (iter-ator:p-array iterator)
      (1+ (flhat:iterator-position iterator))
      0))
(defparameter *attrib-buffers* (fill-with-flhats (make-attrib-buffer-data)))
(defparameter *attrib-buffer-iterators*
  (make-iterators *attrib-buffers* (make-attrib-buffer-data)))
(defparameter *attrib-buffer-fill-pointer*
  (tally-buffer *attrib-buffer-iterators* (make-attrib-buffer-data)))

(defparameter *default-tex-params* (quote ((:texture-min-filter . :nearest)
					   (:texture-mag-filter . :nearest)
					   (:texture-wrap-s . :repeat)
					   (:texture-wrap-t . :repeat))))

(defparameter *16x16-tilemap* (regular-enumeration 16 16))

(progn
  (defparameter *block-height* nil)
  (defparameter *block-width* nil)
  (setf (values *block-height* *block-width*)
	(case 0
	  (0 (values 16.0 9.0))
	  (1 (values 11.0 6.0))
	  (2 (values 16.0 16.0))))
  (setf *block-height* (* *block-height* 2.0)
	*block-width* (* *block-width* 2.0)))

(defparameter *backup* (make-eq-hash))
(defparameter *stuff* (make-eq-hash))
(defparameter *other-stuff* (make-eq-hash))

(defparameter vsync? t)

(defun render ()
  (let ((now (get-internal-real-time)))
    
    (setf *last-time* now))
  (draw-things)

  (window:update-display))

(defparameter *window-width* 64) ;;random numbers which change
(defparameter *window-height* 64);;

(defparameter *window-block-width* 16)
(defparameter *window-block-height* 16)
(defparameter *last-time* 0)

(defun update-window-block-size ()
  (setf (values *window-block-width* *window-block-height*)
	(values
	 (ceiling (/ e:*width* *block-width* 0.5))
	 (ceiling (/ e:*height* *block-height* 0.5)))))

(defun draw-things ()
  (progn
    (let* ((solidshader (get-stuff :other-text-shader *stuff* *backup*))
	   (solidshader-uniforms (glget solidshader :program))
	   (sampler2d (getuniform solidshader-uniforms :sampler-2d))
	   (indirection (getuniform solidshader-uniforms :indirection)))
      (gl:use-program solidshader)
      (gl:uniformi sampler2d 1)
      (gl:active-texture (+ 1 +gltexture0+))
      (gl:bind-texture :texture-2d (get-stuff :font *stuff* *backup*))
      
      (gl:uniformi indirection 0)
      (gl:active-texture (+ 0 +gltexture0+))    
      (gl:bind-texture :texture-2d (get-stuff :text-scratch *stuff* *backup*))
      
      (gl:call-list (get-stuff :fast-text-display-list *stuff* *backup*)))))

(defun reset-text-display-list ()
  (let ((list (get-stuff :fast-text-display-list *stuff* *backup*)))
    (when list
      (gl:delete-lists list 1)
      (remhash :fast-text-display-list *stuff*))))

(defmacro with-iterators ((&rest bufvars) buf func type &body body)
  (let* ((syms (mapcar (lambda (x) (gensym (string x))) bufvars))
	 (bindings (mapcar (lambda (x y) (list x y))
			   bufvars syms))
	 (decl `(declare (type ,type ,@syms))))
    (with-vec-params syms `(,buf)
      decl
      `(,func ,bindings
	      ,@body))))

(defun glinnit ()
  (reset *gl-objects*)
  (setf %gl:*gl-get-proc-address* (e:get-proc-address))

  (if vsync?
      (window::set-vsync t)
      (window::set-vsync nil))
  
  (let ((width (if t 480 854))
	(height (if t 360 480)))
    (window:push-dimensions width height))
  (setf e:*resize-hook* #'on-resize)

  (progn
    (gl:disable :depth-test :blend)
    (gl:depth-mask :false)
    (gl:depth-func :always)
    (gl:disable :cull-face))

  (let ((hash *stuff*))
    (maphash (lambda (k v)
	       (if (integerp v)
		   (remhash k hash)))
	     hash))
  (let ((backup *backup*))
    (progn
      
      (namexpr backup :text-frag
	       (lambda () (file-string (shader-path "ftex2f-bg4f-fg4f.frag"))))

      (namexpr backup :text-indirect
	       (lambda () (file-string (shader-path "text69.vs"))))
      (namexpr backup :other-text-shader
	       (lambda ()
		 (let ((program
			(make-shader-program-from-strings
			 (get-stuff :text-indirect *stuff* *backup*)
			 (get-stuff :text-frag *stuff* *backup*)
			 (quote (("POS" . 0)	
				 ("TEX" . 8)
				 ("INDIRECT" . 9)
				 )))))
		   (let ((table (make-eq-hash)))
		     (register program :program table)
		     (cache-program-uniforms program table (quote ((:sampler-2d . "samptwodee")
								   (:indirection . "indirection")
								   (:texcoord . "texcoords")
								   (:fgcolor . "fgcolor")
								   (:bgcolor . "bgcolor")
								   )))
		     (let* ((solidshader program)
			    (solidshader-uniforms (glget solidshader :program)))
		       (progn
			 (gl:use-program program)
			 (%gl:uniform-4fv (getuniform solidshader-uniforms :texcoord)
					  256
					  (get-stuff :glsl-code-lookup *other-stuff* *backup*))
			 (let ((color-lookup (get-stuff :terminal256color-lookup *other-stuff* *backup*)))
			   (%gl:uniform-4fv (getuniform solidshader-uniforms :fgcolor)
					    256
					    color-lookup)
			   (%gl:uniform-4fv (getuniform solidshader-uniforms :bgcolor)
					    256
					    color-lookup)))))
		   program))))
    
    (progn
      (namexpr backup :font-image
	       (lambda ()
		 (flip-image
		  (load-png
		   (img-path "font/font.png")))))
      (namexpr backup :font
	       (lambda ()
		 (pic-texture (get-stuff :font-image *stuff* *backup*)
			      :rgba
			      *default-tex-params*))))
    (namexpr backup :glsl-code-lookup
	     (lambda ()
	       (let ((a (cffi:foreign-alloc :float :count (* 4 256))))
		 (dotimes (x 256)
		   (let ((offset (* 4 x))
			 (tilemap-lookup *16x16-tilemap*))
		     (etouq
		      (with-vec-params
			  `((offset ,@(vec-slots :rectangle
						 '((x0 :x0) (y0 :y0) (x1 :x1) (y1 :y1)))))
			'(tilemap-lookup)
			'(progn
			  (setf (cffi:mem-aref a :float (+ offset 0)) x0)
			  (setf (cffi:mem-aref a :float (+ offset 1)) y0)
			  (setf (cffi:mem-aref a :float (+ offset 2)) x1)
			  (setf (cffi:mem-aref a :float (+ offset 3)) y1))))))
		 a)))
    (namexpr backup :terminal256color-lookup
	     (lambda ()
	       (let ((a (cffi:foreign-alloc :float :count (* 4 256))))
		 (dotimes (x 256)
		   (let ((offset (* 4 x)))
		     (multiple-value-bind (r g b) (color-rgb x) 
			 (progn
			   (setf (cffi:mem-aref a :float (+ offset 0)) r)
			   (setf (cffi:mem-aref a :float (+ offset 1)) g)
			   (setf (cffi:mem-aref a :float (+ offset 2)) b)
			   (setf (cffi:mem-aref a :float (+ offset 3)) 0f0)))))
		 a)))
    (namexpr backup :glyph-screen
	     (lambda ()
	       (cffi:foreign-alloc :uint8 :count (* 256 256 4))))
    (progn
      (namexpr backup :items-image
	       (lambda ()
		 (flip-image
		  (load-png
		   (img-path "font/items.png")))))
      (namexpr backup :items
	       (lambda ()
		 (pic-texture (get-stuff :items-image *stuff* *backup*)
			      :rgba
			      *default-tex-params*)))
      (namexpr backup :text-scratch
	       (lambda ()
		 (pic-texture (get-stuff :items-image *stuff* *backup*)
			      :rgba
			      *default-tex-params*)))
      (namexpr backup :fast-text-display-list
	       (lambda ()
		 (draw-fast-text-display-list
		  (/ *window-width* *block-width* 0.5)
		  (/ *window-height* *block-height* 0.5)))))
    (progn
      (namexpr backup :terrain-image
	       (lambda ()
		 (flip-image
		  (load-png
		   (img-path "font/terrain.png")))))
      (namexpr backup :terrain
	       (lambda ()
		 (pic-texture (get-stuff :terrain-image *stuff* *backup*)
			      :rgba
			      *default-tex-params*))))))

(defun cache-program-uniforms (program table args)
  (dolist (arg args)
    (setf (gethash (car arg) table)
	  (gl:get-uniform-location program (cdr arg)))))
(defun getuniform (shader-info name)
  (gethash name shader-info))

(defun on-resize (w h)
  (setf *window-height* h
	*window-width* w)
  (update-window-block-size)
  (reset-text-display-list)
  (gl:viewport 0 0 w h))

(defparameter *buffer-vector-scratch* (make-array 16))

(defun get-buf-param (iter attrib-order &optional (newarray *buffer-vector-scratch*))
  (let ((len (length attrib-order)))
    (dotimes (x len)
      (setf (aref newarray x)
	    (aref iter (aref attrib-order x)))))
  newarray)


(defun draw-fast-text (bufs
		       width height
		       xoffset yoffset
		       z)
  (let ((xwidth (/ 2.0 width))
	(ywidth (/ 2.0 height)))
    (let ((upwidth (ceiling width))
	  (upheight (ceiling height)))
      (with-iterators (epos etex eindirect) bufs iter-ator:wasabios iter-ator:iter-ator
	(dobox ((xcell 0 upwidth)
		(ycell 0 upheight))
	   ;;;texcoords
	       (etouq (ngorp (preach 'etex (duaq 3 nil '(0f0 1f0 0f0 1f0)))))
	       (let ((xactual (- (* xwidth xcell) 1.0))
		     (yactual (- (* ywidth ycell) 1.0)))
		 (let* ((x1 (float xactual))
			(y1 (float yactual))
			(foox0 (+ x1 xwidth))
			(fooy0 (+ y1 ywidth)))
		   ;;position
		   (etouq (ngorp (preach 'epos (quadk+ 'z '(foox0 x1 fooy0 y1)))))))
	   ;;;indirection
	       (let ((xi (* xoffset xcell))
		     (yi (* yoffset ycell)))
		 (dotimes (x 4)
		   (progn
		     (eindirect xi)
		     (eindirect yi))))))
      (* 4 upwidth upheight))))

(defun draw-fast-text-display-list (width height &optional (display-list (gl:gen-lists 1)))
  (declare (optimize (debug 3)))
  (let ((iter *attrib-buffer-iterators*))
    (let ((buf (get-buf-param iter
			      (etouq (vector 0 8 9)))))
      (reset-attrib-buffer-iterators iter)
      (let ((times (draw-fast-text
		    buf
		    width height
		    (/ 1.0 256) (/ 1.0 256)
		    0.0)))
	(reset-attrib-buffer-iterators iter)
	(gl:with-new-list (display-list :compile)
	  (gl:with-primitives :quads
	    (mesh-test89 times buf)))
	display-list))))

(defun mesh-test89 (times bufs)
  (with-iterators (xyz uv eindirect) bufs iter-ator:wasabiis iter-ator:iter-ator
    (dotimes (x times)
      (%gl:vertex-attrib-2f 8 (uv) (uv))
      (%gl:vertex-attrib-2f 9 (eindirect) (eindirect))
      (%gl:vertex-attrib-3f 0 (xyz) (xyz) (xyz)))))
