(in-package :sandbox)


(defparameter vsync? t)

(defparameter *mat4-identity* (cg-matrix:identity-matrix))

(defconstant +single-float-just-less-than-one+ 0.99999997)

(defparameter *16x16-tilemap* (regular-enumeration 16 16))
(defparameter *4x4-tilemap* (regular-enumeration 4 4))

(defparameter *screen-scaled-matrix* (cg-matrix:identity-matrix))



(defparameter *gl-objects* (make-meta-globject))

(defparameter *attrib-buffers* (fill-with-flhats (make-attrib-buffer-data)))
(defparameter *attrib-buffer-iterators*
  (make-iterators *attrib-buffers* (make-attrib-buffer-data)))
(defparameter *attrib-buffer-fill-pointer*
  (tally-buffer *attrib-buffer-iterators* (make-attrib-buffer-data)))

(defparameter *backup* (make-hash-table :test (quote eq)))
(defparameter *stuff* (make-hash-table :test (quote eq)))

(defparameter *default-tex-params* (quote ((:texture-min-filter . :nearest)
					   (:texture-mag-filter . :nearest)
					   (:texture-wrap-s . :repeat)
					   (:texture-wrap-t . :repeat))))

(defun render ()
  (if vsync?
      (window::set-vsync t)
      (window::set-vsync nil))
  (draw-things)

  (window:update-display))

(defparameter foo
  (make-array 0 :adjustable t :fill-pointer 0
	      :element-type (quote character)))

(defun draw-things () 
  (let ((solidshader (get-stuff :solidshader *stuff* *backup*)))
    (gl:viewport 0 0 e:*width* e:*height*)
    (gl:use-program solidshader)
    (cg-matrix:%scale* *screen-scaled-matrix* (/ 1.0 e:*width*) (/ 1.0 e:*height*) 1.0) 
    (gl:uniform-matrix-4fv
     (gl:get-uniform-location solidshader "pmv")
     (cg-matrix:%matrix* *temp-matrix2*
			 *screen-scaled-matrix*
			 (cg-matrix:%translate* *temp-matrix*
						(float (- e:*width*))
						(float e:*height*)
						0.0))
     nil)

    (progn
      (gl:disable :depth-test :blend)
      (gl:depth-mask :false)
      (gl:depth-func :always)
      (gl:clear-color 0.0 0.0 0.0 0f0)
      (gl:clear
       :color-buffer-bit 
       ))

    (progn
      (gl:bind-texture :texture-2d (get-stuff :font *stuff* *backup*))
      (progn (let ((scale 32.0))
	       (namexpr *backup* :string
			(lambda ()
			  (create-call-list-from-func
			   (lambda ()
			     (gl-draw-quads 
			      (lambda (tex-buf pos-buf lit-buf)
				(draw-string-raster-char
				 pos-buf tex-buf lit-buf
				 *16x16-tilemap* foo
				 (/ scale 2.0)
				 (/ scale 1.0)
				 0.0 0.0
				 +single-float-just-less-than-one+)))))))))

      (let ((newlen (length e:*chars*))
	    (changed nil))
	(dotimes (pos newlen)
	  (vector-push-extend (aref e:*chars* pos) foo))
	(cond ((e:key-j-p (cffi:foreign-enum-value (quote %glfw::key) :enter))
	       (multiple-value-bind (data p) (read-string foo nil)
		 (cond (p (print data)
			  (multiple-value-list
			   (handler-bind ((condition (lambda (c)
						       (declare (ignorable c))
						       (invoke-restart
							(find-restart (quote continue))))))
			       (restart-case
				   (eval data)
				 (continue () data))))
			  (setf (fill-pointer foo) 0))
		       (t (vector-push-extend #\Newline foo))))
	       (setf changed t)))
	(cond ((e:key-j-p (cffi:foreign-enum-value (quote %glfw::key) :backspace))
	       (setf (fill-pointer foo) (max 0 (1- (fill-pointer foo))))
	       (setf changed t)))
	(when (or changed (not (zerop newlen)))
	  (let ((list (get-stuff :string *stuff* *backup*)))
	    (gl:delete-lists list 1)
	    (remhash :string *stuff*))))
      (gl:call-list (get-stuff :string *stuff* *backup*)))))

(defun glinnit ()
  (reset *gl-objects*)
  (setf %gl:*gl-get-proc-address* (e:get-proc-address))
  
  (let ((width (if t 480 854))
	(height (if t 360 480)))
    (window:push-dimensions width height))
  (setf e:*resize-hook* #'on-resize)

  (let ((hash *stuff*))
    (maphash (lambda (k v)
	       (if (integerp v)
		   (remhash k hash)))
	     hash))
  (let ((backup *backup*))
    (progn    
      (namexpr backup :solidshader
	       (lambda ()
		 (make-shader-program-from-strings
		  (get-stuff :ss-vs *stuff* *backup*)
		  (get-stuff :ss-frag *stuff* *backup*)
		  '(("pos" . 0)	
		    ("col" . 3)
		    ("tex" . 8)))))
      (namexpr backup :ss-vs
	       (lambda () (file-string (shader-path "pos4f-col3f-tex2f.vs"))))
      (namexpr backup :ss-frag
	       (lambda () (file-string (shader-path "fcol3f-ftex2f-no0a.frag")))))
    
    (progn
      (namexpr backup :font-image
	       (lambda ()
		 (flip-image
		  (load-png
		   (img-path #P"font/codepage-437-vga-9x16-alpha.png")))))
      (namexpr backup :font
	       (lambda ()
		 (pic-texture (get-stuff :font-image *stuff* *backup*)
			      :rgba
			      *default-tex-params*))))
    
    (progn
      (namexpr backup :cursor-image
	       (lambda ()
		 (flip-image
		  (load-png
		   (img-path #P"cursor/windos-cursor.png")))))
      (namexpr backup :cursor
	       (lambda ()
		 (pic-texture (get-stuff :cursor-image *stuff* *backup*)
			      :rgba
			      *default-tex-params*))))))

(defun on-resize (w h)
  (setf *window-height* h
	*window-width* w))

(defun gl-draw-quads (func)
  (let ((iter *attrib-buffer-iterators*))
    (reset-attrib-buffer-iterators iter)
    (let ((pos-buf (aref iter 0))
	  (lit-buf (aref iter 3))
	  (tex-buf (aref iter 8)))
      (let ((times (funcall func tex-buf pos-buf lit-buf)))
	(gl:with-primitives :quads
	  (reset-attrib-buffer-iterators iter)
	  (mesh-test42 times lit-buf tex-buf pos-buf))))))

(defun mesh-test42 (times lit tex pos)
  (declare (type iter-ator:iter-ator tex pos))
  (iter-ator:wasabiis ((d lit)
		       (uv tex)
		       (xyz pos))
    (dotimes (x times)
      (%gl:vertex-attrib-2f 8 (uv) (uv))
      (%gl:vertex-attrib-3f 3 (d) (d) (d))
      (%gl:vertex-attrib-3f 0 (xyz) (xyz) (xyz)))))

(defun draw-mouse (pos-buf tex-buf lit-buf
		   lookup value char-width char-height x y z)
  (declare (type iter-ator:iter-ator pos-buf tex-buf lit-buf)
	   (type single-float x y z char-width char-height)
	   (type simple-vector lookup)
	   (optimize (speed 3) (safety 0))
	   (type fixnum value))
  (iter-ator:wasabios ((epos pos-buf)
		       (etex tex-buf)
		       (elit lit-buf))

    (dotimes (x 4)
      (etouq (ngorp (preach 'elit '(1f0
				    1f0
				    1f0)))))
    (multiple-value-bind (x0 y0 x1 y1) (index-quad-lookup lookup value)
      (etouq (ngorp (preach 'etex (duaq 1 nil '(x0 x1 y0 y1))))))
    (etouq (ngorp
	    (preach
	     'epos
	     (quadk+ 'z '(x
			  (+ char-width x)
			  (- y char-height)
			  y)))))4))

(defun draw-string-raster-char (pos-buf tex-buf lit-buf
				lookup string char-width char-height x y z)
  (declare (type iter-ator:iter-ator pos-buf tex-buf lit-buf)
	   (type single-float x y z char-width char-height)
	   (type simple-vector lookup)
	   (optimize (speed 3) (safety 0))
	   (type (vector character) string))
  (iter-ator:wasabios ((epos pos-buf)
		       (etex tex-buf)
		       (elit lit-buf))
    (let ((len (length string))
	  (times 0))
      (declare (type fixnum times))
      (let ((xoffset x)
	    (yoffset y))
	(declare (type single-float xoffset yoffset))
	(let ((wonine (if nil 0.0 (/ char-width 8.0))))
	  (dotimes (position len)
	    (let ((char (row-major-aref string position)))
	      (let ((next-x (+ xoffset char-width wonine))
		    (next-y (- yoffset char-height)))
		(if (char= char #\Newline)
		    (setf xoffset x
			  yoffset next-y)
		    (progn
		      (unless (char= #\space char)
			(incf times 4)
			(dotimes (x 4)
			  (etouq (ngorp (preach 'elit '(0f0
							1f0
							0f0
							)))))
			(let ((code (char-code char)))
			  (multiple-value-bind (x0 y0 x1 y1) (index-quad-lookup lookup code)
			    (etouq (ngorp (preach 'etex (duaq 1 nil '(x0 x1 y0 y1)))))))
			(etouq (ngorp
				(preach
				 'epos
				 (quadk+ 'z '(xoffset
					      (- next-x wonine)
					      next-y
					      yoffset))))))
		      (setf xoffset next-x))))))))
      times)))

(defun render-mouse ()
      (progn
	(gl:uniform-matrix-4fv
	 (gl:get-uniform-location (get-stuff :solidshader *stuff* *backup*) "pmv")
	 (cg-matrix:%matrix* *temp-matrix2*
			     *screen-scaled-matrix*
			     (cg-matrix:%translate* *temp-matrix* cursor-x cursor-y 0.0))
	 nil)
	(progn
	  (gl:bind-texture :texture-2d (get-stuff :cursor *stuff* *backup*))
	  (progn (let ((scale 64.0))
		   (namexpr *backup* :cursor-list
			    (lambda ()
			      (create-call-list-from-func
			       (lambda ()
				 (gl-draw-quads 
				  (lambda (tex-buf pos-buf lit-buf)
				    (draw-mouse
				     pos-buf tex-buf lit-buf
				     *4x4-tilemap* 0
				     scale 
				     scale
				     -0.0 0.0
				     (- +single-float-just-less-than-one+))))))))))
	  (gl:call-list (get-stuff :cursor-list *stuff* *backup*)))))

(defun quit ()
  (setf e:*status* t))


(defun goo (c)
  (declare (ignorable c))
  (let ((restart (find-restart (quote goober))))
    (when restart (invoke-restart restart))))

(defun read-string (string otherwise)
  (handler-bind ((end-of-file #'goo))
    (restart-case
	(values (read-from-string string nil otherwise) t)
      (goober ()
	:report "wtf"
	(values otherwise nil)))))
