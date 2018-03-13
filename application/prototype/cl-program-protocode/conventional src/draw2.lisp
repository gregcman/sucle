(in-package :aplayground)

(progn
  (progn
    (defparameter *block-height* nil)
    (defparameter *block-width* nil)
    (setf (values *block-height* *block-width*)
	  (case 4
	    (0 (values 16.0 9.0))
	    (4 (values 16.0 8.0))
	    (1 (values 11.0 6.0))
	    (2 (values 16.0 16.0))))
    (setf *block-height* (* *block-height* 2.0)
	  *block-width* (* *block-width* 2.0)))
  (defparameter *window-block-width* 16)
  (defparameter *window-block-height* 16)
  (defun update-window-block-size ()
    (setf (values *window-block-width* *window-block-height*)
	  (values
	   (ceiling (/ e:*width* *block-width* 0.5))
	   (ceiling (/ e:*height* *block-height* 0.5))))))

(bornfnc
 :text-frag
 (lambda () (alexandria:read-file-into-string (shader-path "ftex2f-bg4f-fg4f.frag"))))
(bornfnc
 :text-indirect
 (lambda () (alexandria:read-file-into-string (shader-path "text69.vs"))))
(bornfnc
 :other-text-shader
 (lambda ()
   (let ((program
	  (lovely-shader-and-texture-uploader:make-shader-program-from-strings
	   (get-stuff :text-indirect *stuff* *backup*)
	   (get-stuff :text-frag *stuff* *backup*)
	   (quote (("POS" . 0)	
		   ("TEX" . 8)
		   ("INDIRECT" . 9)
		   )))))
     (let ((table (make-hash-table :test 'eq)))
       (setf *solidshader-uniforms* table)
       (cache-program-uniforms program table (quote ((:sampler-2d . "samptwodee")
						     (:indirection . "indirection")
						     (:texcoord . "texcoords")
						     (:fgcolor . "fgcolor")
						     (:bgcolor . "bgcolor")
						     )))
       (let* (;;(solidshader program)
	      (solidshader-uniforms table))
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
     program)))
(defparameter *solidshader-uniforms* nil)

(progn
  (bornfnc
   :font-image
   (lambda ()
     (load-png
      (img-path "font/shitty.png"))))
  (bornfnc
   :font
   (lambda ()
     (prog1
	 (lovely-shader-and-texture-uploader:pic-texture
	  (get-stuff :font-image *stuff* *backup*)
	  :luminance)
       (lovely-shader-and-texture-uploader::apply-tex-params
	lovely-shader-and-texture-uploader::*default-tex-params*)))))
#+nil
(progn
  (bornfnc
   :font-image2
   (lambda ()
     (flip-image
      (load-png
       (img-path "font/achar.png")))))
  (bornfnc
   :font2
   (lambda ()
     (lovely-shader-and-texture-uploader:pic-texture
      (get-stuff :font-image2 *stuff* *backup*)
      :rgba
      *default-tex-params*))))
(bornfnc
 :glyph-screen
 (lambda ()
   (cffi:foreign-alloc :uint8 :count (* 256 256 4))))
(progn
  #+nil
  (bornfnc
   :items-image
   (lambda ()
     (flip-image
      (load-png
       (img-path "font/items.png")))))
  #+nil
  (bornfnc
   :items
   (lambda ()
     (lovely-shader-and-texture-uploader:pic-texture
      (get-stuff :items-image *stuff* *backup*)
      :rgba
      *default-tex-params*)))
  (bornfnc
   :text-scratch
   (lambda ()
     (prog1
	 (let ((tex (gl:gen-texture)))
	   (gl:bind-texture :texture-2d tex)
	   (gl:tex-image-2d :texture-2d 0 :rgba 256 256 0
			    :rgba :unsigned-byte (cffi:null-pointer))
	   tex)
       (lovely-shader-and-texture-uploader::apply-tex-params
	lovely-shader-and-texture-uploader::*default-tex-params*))))
  (bornfnc
   :fast-text-display-list
   (lambda ()
     (draw-fast-text-display-list
      (/ window::*width* *block-width* 0.5)
      (/ window::*height* *block-height* 0.5)))))
#+nil
(progn
  (bornfnc
   :terrain-image
   (lambda ()
     (flip-image
      (load-png
       (img-path "font/terrain.png")))))
  (bornfnc
   :terrain
   (lambda ()
     (lovely-shader-and-texture-uploader:pic-texture
      (get-stuff :terrain-image *stuff* *backup*)
      :rgba
      *default-tex-params*))))


(defun draw-fast-text (bufs
		       width height
		       xoffset yoffset
		       z)
  (let ((xwidth (/ 2.0 width))
	(ywidth (/ 2.0 height)))
    (let ((upwidth (ceiling width))
	  (upheight (ceiling height)))
      (with-iterators (epos etex eindirect) bufs iter-ator:wasabios
	(dobox ((xcell 0 upwidth)
		(ycell 0 upheight))
	   ;;;texcoords
	       (etouq (ngorp (preach 'etex (axis-aligned-quads:duaq 3 nil '(0f0 0.0626 0f0 0.0625)))))
	       (let ((xactual (- (* xwidth xcell) 1.0))
		     (yactual (- (* ywidth ycell) 1.0)))
		 (let* ((x1 (float xactual))
			(y1 (float yactual))
			(foox0 (+ x1 xwidth))
			(fooy0 (+ y1 ywidth)))
		   ;;position
		   (etouq (ngorp (preach 'epos (axis-aligned-quads:quadk+ 'z '(foox0 x1 fooy0 y1)))))))
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
  (with-iterators (xyz uv eindirect) bufs iter-ator:wasabiis
    (dotimes (x times)
      (%gl:vertex-attrib-2f 8 (uv) (uv))
      (%gl:vertex-attrib-2f 9 (eindirect) (eindirect))
      (%gl:vertex-attrib-3f 0 (xyz) (xyz) (xyz)))))
(defun reset-text-display-list ()
  (let ((list (get-stuff :fast-text-display-list *stuff* *backup*)))
    (when list
      (gl:delete-lists list 1)
      (remhash :fast-text-display-list *stuff*))))

(progn
  (defun render ()
    (progn
      (let* ((solidshader (get-stuff :other-text-shader *stuff* *backup*))
	     (solidshader-uniforms *solidshader-uniforms*)
	     (sampler2d (getuniform solidshader-uniforms :sampler-2d))
	     (indirection (getuniform solidshader-uniforms :indirection)))
	(gl:use-program solidshader)
	(gl:uniformi sampler2d 1)
	(set-active-texture 1)
	(gl:bind-texture :texture-2d (get-stuff :font *stuff* *backup*))
	
	(gl:uniformi indirection 0)
	(set-active-texture 0)    
	(gl:bind-texture :texture-2d (get-stuff :text-scratch *stuff* *backup*))

	(progn
	  (gl:disable :depth-test :blend)
	  (gl:depth-mask :false)
	  (gl:depth-func :always)
	  (gl:disable :cull-face))
	
	(gl:call-list (get-stuff :fast-text-display-list *stuff* *backup*))))))


;;;fixme
(defun on-resize (w h)
  (update-window-block-size)
  (reset-text-display-list))

(defun glinnit ()
  (setf e:*resize-hook* #'on-resize))

#+nil
((bornfnc
	:items-image
	(lambda ()
	  (flip-image
	   (load-png
	    (img-path "font/fuckme.png")))))

       #+nil
       (dotimes (y 16)
	 (terpri)
	 (dotimes (x 16)
	   (princ (code-char (+ (* y 16) x)))))

 (defparameter *woy*
   (lovely-shader-and-texture-uploader::array-flatten
    (get-stuff :items-image *stuff* *backup*)))

 (defparameter *color-hash* (make-hash-table :test 'equalp))

 (dobox ((x 0 (length *woy*) :inc 3))
	(etouq (with-vec-params (quote ((x r g b)))
		 (quote (aplayground::*woy*))
		 (quote
		  (progn
		    (incf (gethash (Vector r g b) *color-hash* 0))
		    (sandbox::goto r g b)
		    (sandbox::what345 r g b 4
				      ))))))

 (defparameter *colors* (make-array '(256 128) :element-type '(unsigned-byte 8)))
 (dobox ((x 0 (length *woy*) :inc 3))
	(etouq (with-vec-params (quote ((x r g b)))
		 (quote (aplayground::*woy*))
		 (quote
		  (progn
		    (setf (row-major-aref *colors* (/ x 3))
			  (floor (* 255 (alpha (vector r g b))))))))))
 (opticl:write-png-file (img-path "font/shitty.png") *colors*)

 (map 'vector *woy*)

 (defun alpha (x)
   (let ((tot (+ (distance x #(255 193 193))
		 (distance x #(0 0 205)))))
     (/ (distance x #(0 0 205))
	tot)))

 (defun average (x)
   (/ (reduce #'+ x)
      (length x)))

 (defun distance (x y)
   (sqrt (reduce #'+
		 (map 'vector (lambda (x) (* x x))
		      (map 'vector #'- x y)))))
 )
