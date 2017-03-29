(in-package :sandbox)

(defparameter *something* #.(or *compile-file-truename* *load-truename*))

(defparameter ourdir
  (make-pathname :host (pathname-host *something*)
		 :directory (pathname-directory *something*)))


(defconstant +single-float-pi+ (coerce pi 'single-float))
(defconstant +single-float-two-pi+ (coerce (* 2 pi) 'single-float))
(defconstant +single-float-half-pi+ (coerce (/ pi 2) 'single-float))

(defun clamp (x min max)
  (max (min x max) min))

(defparameter *temp-matrix* (cg-matrix:identity-matrix))
(defparameter *temp-matrix2* (cg-matrix:identity-matrix))
(defparameter *temp-matrix3* (cg-matrix:identity-matrix))
(defparameter *x-unit* (cg-matrix:vec 1.0 0.0 0.0))

(defun byte-read (path)
   (with-open-file (stream path :element-type '(unsigned-byte 8))
     (let* ((len (file-length stream))
	    (data (make-array len :element-type '(unsigned-byte 8))))
       (dotimes (n len)
	 (setf (aref data n) (read-byte stream)))
       data)))

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun spill-hash (hash)
  (loop for key being the hash-keys of hash
     using (hash-value value)
     do (format t "~S ~S~%" key value)))

(defun print-bits (n)
  (format t "~64,'0b" n))

(defun getapixel (h w image)
  (destructuring-bind (height width c) (array-dimensions image)
    (declare (ignore height))
    (make-array 4 :element-type (array-element-type image)
		:displaced-to image
		:displaced-index-offset (* c (+ w (* h width))))))

;;;;load a png image from a path
(defun load-png (filename)
  (opticl:read-png-file filename))

(defparameter *save* #P"third/")

(defparameter *saves-dir* (merge-pathnames #P"saves/" ourdir))

(defun save (filename &rest things)
  (let ((path (merge-pathnames filename *saves-dir*)))
    (with-open-file (stream path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (dolist (thing things)
	(prin1 thing stream)))))

(defun save2 (thingfilename &rest things)
  (apply #'save (merge-pathnames (format nil "~s" thingfilename) *save*) things))

(defun myload2 (thingfilename)
  (myload (merge-pathnames (format nil "~s" thingfilename) *save*)))

(defun myload (filename)
  (let ((path (merge-pathnames filename *saves-dir*)))
    (let ((things nil))
      (with-open-file (stream path :direction :input :if-does-not-exist nil)
	(tagbody rep
	   (let ((thing (read stream nil nil)))
	     (when thing
	       (push thing things)
	       (go rep)))))
      (nreverse things))))

(progn
  (defparameter *g/image* (make-hash-table :test 'equal))		    ;;raw image arrays
  (defun get-image (name)
    (let ((img (gethash name *g/image*)))
      (if img
	  img
	  (get-image-backup name))))
  (defun set-image (name image-data)
    (setf (gethash name *g/image*) image-data))
  (defun remove-image (name)
    (remhash name *g/image*)))
(progn
  (defparameter *g/image-backup* (make-hash-table :test 'equal))
  (defun get-image-backup (name)
    (let ((image-func (gethash name *g/image-backup*)))
      (when (functionp image-func)
	(let ((ans (funcall image-func name)))
	  (when ans
	    (set-image name ans)))))))

(progn
  (defparameter *g/text* (make-hash-table :test 'equal))   ;;text: sequences of bytes
  (defun get-text (name)
    (let ((text (gethash name *g/text*)))
      (if text
	  text
	  (get-text-backup name))))
  (defun set-text (name text-data)
    (setf (gethash name *g/text*) text-data))
  (defun remove-text (name)
    (remhash name *g/text*)))
(progn
  (defparameter *g/text-backup* (make-hash-table :test 'equal))
  (defun get-text-backup (name)
    (let ((text-func (gethash name *g/text-backup*)))
      (when (functionp text-func)
	(let ((ans (funcall text-func name)))
	  (when ans
	    (set-text name ans)))))))

(defun totally-destroy-package (package)
  (do-symbols (symbol package)
    (let ((home (symbol-package symbol)))
      (when (eql package home)
	(when (fboundp symbol)
	  (fmakunbound symbol))
	(when (boundp symbol)
	  (makunbound symbol)))))
  (delete-package package))

(defparameter shit nil)

(defun test ()
  (dotimes (x (expt 10 4))
    (let ((package (make-package (gensym))))
      (push package shit))))

(defun test2 (symbol)
  (eval
   `(progn
      (declaim (inline ,symbol))
      (defun ,symbol (package)
	(do-symbols (symbol package)
	  (let ((home (symbol-package symbol)))
	    (when (eq package home)
	      (when (fboundp symbol)
		(fmakunbound symbol))
	      (when (boundp symbol)
		(makunbound symbol)))))
	(print package)
	(print package)
	(print package)
	(print package)
	(print package)
	(print package)
	(print package)
	(print package)
	(print package)
	(write package))
      (declaim (notinline ,symbol))
      
      (values
       (lambda (x)
	 (locally (declare (inline ,symbol))
	   (,symbol x)))
       (quote,symbol)))))

(defparameter foo nil)

(defun wot ()
  (setf foo nil)
  (do-symbols (symbol :cl)
    (if (or (boundp symbol) (fboundp symbol))
	(push (symbol-name symbol) foo)))
  (setf foo (sort foo #'string<)))

(defun simple-defun-p (form)
  (and (eq (quote defun) (pop form))
       (symbolp (pop form))))

(defun fmakunbounds (symbol-list)
  (dolist (symbol symbol-list)
    (fmakunbound symbol)))

(defun makunbounds (symbol-list)
  (dolist (symbol symbol-list)
    (makunbound symbol)))

(defmacro xfmakunbounds (&body symbols)
  `(fmakunbounds (quote ,symbols)))

(defmacro xmakunbounds (&body symbols)
  `(makunbounds (quote ,symbols)))

(defun complex-modulus (c)
  (sqrt (realpart (* c (conjugate c)))))

;;;;flip an image in-place - three dimensions - does not conse
(defun flip-image (image)
  (let ((dims (array-dimensions image)))
    (let ((height (pop dims))
	  (width (pop dims)))
      (if dims
	  (let ((components (car dims)))
	    (dobox ((h 0 (- height (ash height -1)))
		    (w 0 width)
		    (c 0 components))
		   (rotatef (aref image (- height h 1) w c)
			    (aref image h w c))))
	  (dobox ((h 0 (- height (ash height -1)))
		  (w 0 width))
	      (rotatef (aref image (- height h 1) w)
		       (aref image h w))))))
  image)


(defparameter dir-resource (merge-pathnames #P"res/" ourdir))
(defparameter dir-shader (merge-pathnames #P"shaders/" dir-resource))

(defun shader-path (name)
  (merge-pathnames name dir-shader))

(defun img-path (name)
  (merge-pathnames name dir-resource))

(defun name-mesh (display-list-name mesh-func)
  (setf (gethash display-list-name *g/call-list-backup*)
	(lambda (&optional name)
	  (declare (ignorable name))
	  (create-call-list-from-func mesh-func))))

(defun texture-imagery (texture-name image-name)
  (setf (gethash texture-name *g/texture-backup*)
	(lambda (&optional name)
	  (declare (ignorable name))
	  (pic-texture (get-image image-name)))))

(defun name-shader (shader-name vs fs attributes)
  (setf (gethash shader-name *g/shader-backup*)
	(lambda (&optional name)
	  (declare (ignorable name))
	  (make-shader-program-from-strings
	   (get-text vs) (get-text fs) attributes))))

(defun src-image (name src-path)
  (setf (gethash name *g/image-backup*)
	(lambda (&optional name)
	  (declare (ignorable name))
	  (let ((img (load-png src-path)))
	    (flip-image img)
	    img))))

(defun src-text (name src-path)
  (setf (gethash name *g/text-backup*)
	(lambda (&optional name)
	  (declare (ignorable name))
	  (file-string src-path))))


(defparameter foo
  (let ((a (write-to-string
	    '(defun render ()
	      (setf (camera-aspect-ratio *camera*) (/ window:*width* window:*height* 1.0))
	      (if vsync?
		  (window::set-vsync t)
		  (window::set-vsync nil))
	      (update-matrices *camera*)
	      (luse-shader :blockshader)
	      (set-overworld-fog *daytime*)


	      (bind-default-framebuffer)
	      (gl:uniform-matrix-4fv
	       (gl:get-uniform-location *shader-program* "projectionmodelview")
	       *mat4-identity*)
	      (gl:viewport 0 0 e:*width* e:*height*)
	      (setf *aspect-ratio* (/ e:*height* e:*width*))
	      (bind-shit :font)
	      (gl:enable :depth-test)
	      (set-sky-color)
	      
	      (gl:clear :color-buffer-bit :depth-buffer-bit)
	      (lcalllist-invalidate :string)

	      (let ((scale 32.0))
		(name-mesh :string (lambda ()
				     (gl-draw-quads 
				      (lambda (tex-buf pos-buf lit-buf)
					(draw-string-raster-char
					 pos-buf tex-buf lit-buf
					 foo
					 (/ scale e:*width* 2.0)
					 (/ scale e:*height*)
					 -1.0 0.0
					 (- +single-float-just-less-than-one+)))))))
	      (ldrawlist :string)

	      (gl:uniform-matrix-4fv
	       (gl:get-uniform-location *shader-program* "projectionmodelview")
	       (camera-matrix-projection-view-player *camera*)
	       nil)
	      (set-sky-color)
	      
	      (bind-shit :ocean)
	      
	      (ldrawlist :skybox)
	      
	      (window:update-display)))))
    (map-into a
	      (lambda (x) (char-downcase x)) a)))
