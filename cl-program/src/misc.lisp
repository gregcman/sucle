(in-package :sandbox)

(fuktard:eval-always
 (progn
   (defun preach (value form)
     (mapcar (lambda (x)
	       (list value x))
	     form))

   (defun raps (times form)
     (make-list times :initial-element form))

   (defun ngorp (&rest forms)
     (cons (quote progn)
	   (apply (function nconc) forms)))
   (defun ensure (place otherwise)
     (let ((value-var (gensym))
	   (exists-var (gensym)))
       `(or ,place
	    (multiple-value-bind (,value-var ,exists-var) ,otherwise
	      (if ,exists-var
		  (values (setf ,place ,value-var) ,exists-var))))))))

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
 
(defun spill-hash (hash &optional (stream *standard-output*))
  (loop for key being the hash-keys of hash
     using (hash-value value)
     do (format stream "~S ~S~%" key value)))

(defun print-bits (n &optional (stream *standard-output*))
  (format stream "~64,'0b" n)
  n)

(defun getapixel (h w image)
  (destructuring-bind (height width c) (array-dimensions image)
    (declare (ignore height))
    (make-array 4 :element-type (array-element-type image)
		:displaced-to image
		:displaced-index-offset (* c (+ w (* h width))))))

;;;;load a png image from a path
(defun load-png (filename)
  (opticl:read-png-file filename))

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

(defun namexpr (hash name func)
  (setf (gethash name hash) func))

(defun get-stuff (name stuff otherwise)
  (etouq
   (ensure (quote (gethash name stuff))
	   (quote (let ((genfunc (gethash name otherwise)))
		    (when (functionp genfunc)
		      (values (funcall genfunc) t)))))))

(progn

  (defparameter *saves-dir* (merge-pathnames #P"save/" ourdir))
  (defparameter *save-file* "file")

  (defun asave (thing &optional (file *save-file*))
    (save file thing))

  (defun aload (&optional (file *save-file*))
    (myload file))

  (defun save (filename thing &optional (overwritep nil))
    (let ((path (saves-path filename)))
      (with-open-file (stream path
			      :direction :output
			      :if-does-not-exist :create
			      :if-exists (if overwritep :supersede :error)
			      :element-type '(unsigned-byte 8))
	(conspack:encode thing :stream stream))))

  (defun myload (filename)
    (let ((path (saves-path filename)))
      (conspack:decode (byte-read path)))))
(defun saves-path (path)
  (merge-pathnames path *saves-dir*))


(defun quit ()
  (setf e:*status* t))

(progn
  (progn
    (defun skey-p (enum)
      (e:key-p enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::key))
	       ))
    (defun skey-j-r (enum)
      (e:key-j-r enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::key))
		 ))
    (defun skey-j-p (enum)
      (e:key-j-p enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::key))
		 ))
    (defun smice-p (enum)
      (e:mice-p enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::mouse))
		))
    (defun smice-j-p (enum)
      (e:mice-j-p enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::mouse))
		  ))
    (defun skey-r-or-p (enum)
      (e:key-r-or-p enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::key))
		    ))
    (defun smice-r-or-p (enum)
      (e:mice-r-or-p enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::mouse))
		     ))))

(defun make-eq-hash ()
  (make-hash-table :test (quote eq)))

(progn
  (defun floor-chunk (x y)
    (* y (floor x y)))

  (defun acolor (&rest values)
    (setf values (nreverse values))
    (let ((acc 0))
      (dolist (value values)
	(setf acc (ash (logior acc value) 8)))
      (logand acc most-positive-fixnum)))

  (progn
    (declaim (inline byte-color)
	     (ftype (function (fixnum) single-float)
		    byte-color))
    (with-unsafe-speed
      (defun byte-color (x)
	(/ (float x) 255.0))))

  (defun strip-char (color)
    (logandc1 255 color))

  (defparameter *white-black-color* (acolor 255 255 255 0 0 0))
  (defparameter *color-nil* (logandc1 255 (sxhash nil))))
