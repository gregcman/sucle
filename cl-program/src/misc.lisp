(in-package :aplayground)

(fuktard:eval-always
 (progn
   (defun preach (value form)
     (mapcar (lambda (x)
	       (list value x))
	     form))

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

;;;;load a png image from a path
(defun load-png (filename)
  (opticl:read-png-file filename))


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

(defun namexpr (hash name func)
  (setf (gethash name hash) func))
(defun get-stuff (name stuff otherwise)
  (etouq
   (ensure (quote (gethash name stuff))
	   (quote (let ((genfunc (gethash name otherwise)))
		    (when (functionp genfunc)
		      (values (funcall genfunc) t)))))))

(defparameter *backup* (make-hash-table :test 'eq))
(defparameter *stuff* (make-hash-table :test 'eq))

(defun bornfnc (name func)
  (namexpr *backup* name func))

(defun getfnc (name)
  (get-stuff name *stuff* *backup*))

(defun cache-program-uniforms (program table args)
  (dolist (arg args)
    (setf (gethash (car arg) table)
	  (gl:get-uniform-location program (cdr arg)))))

(defun getuniform (shader-info name)
  (gethash name shader-info))

#+nil
(defparameter *other-stuff* (make-hash-table :test 'eq))

#+nil
(defparameter *something* #.(or *compile-file-truename* *load-truename*))
#+nil
(defparameter ourdir
  (make-pathname :host (pathname-host *something*)
		 :directory (pathname-directory *something*)))


#+nil
  (defun raps (times form)
     (make-list times :initial-element form))

#+nil
(progno
 (defconstant +single-float-pi+ (coerce pi 'single-float))
 (defconstant +single-float-two-pi+ (coerce (* 2 pi) 'single-float))
 (defconstant +single-float-half-pi+ (coerce (/ pi 2) 'single-float)))

#+nil
(defun clamp (x min max)
  (max (min x max) min))

#+nil
(progno
 (defparameter *temp-matrix* (cg-matrix:identity-matrix))
 (defparameter *temp-matrix2* (cg-matrix:identity-matrix))
 (defparameter *temp-matrix3* (cg-matrix:identity-matrix))
 (defparameter *x-unit* (cg-matrix:vec 1.0 0.0 0.0)))


#+nil
(progno
 (defparameter dir-resource (merge-pathnames #P"res/" ourdir))
 (defparameter dir-shader (merge-pathnames #P"shaders/" dir-resource))

 (defun shader-path (name)
   (merge-pathnames name dir-shader))

 (defun img-path (name)
   (merge-pathnames name dir-resource)))



#+nil
(progno
 (progn
   (defparameter *saves-dir* (merge-pathnames #P"save/" ourdir))
   (defparameter *save-file* "file")

   (defun asave (thing &key (file *save-file*) (overwritep nil))
     (save file thing overwritep))

   (defun aload (&optional (file *save-file*))
     (myload file))

   (defun save (filename thing &optional (overwritep nil))
     (let ((path (saves-path filename)))
       (with-open-file (stream path
			       :direction :output
			       :if-does-not-exist :create
			       :if-exists (if overwritep :supersede :error)
			       :element-type '(unsigned-byte 8))
	 (conspack:tracking-refs ()
	   (conspack:encode thing :stream stream)))))

   (defun myload (filename)
     (let ((path (saves-path filename)))
       (conspack:tracking-refs ()
	 (conspack:decode (alexandria:read-file-into-byte-vector path))))))
 (defun saves-path (path)
   (merge-pathnames path *saves-dir*)))

#+nil
(defun quit ()
  (setf e:*status* t))

#+nil
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
		  ))))

#+nil
(defun make-eq-hash ()
  (make-hash-table :test (quote eq)))

#+nil
(progn
  (declaim (ftype (function (symbol t)) set-symbol-value))
  (with-unsafe-speed
    (defun set-symbol-value (symbol value)
      #+sbcl (sb-impl::%set-symbol-value symbol value)
      #-sbcl (set symbol value))))


#+nil
(defun byte-read (path)
   (with-open-file (stream path :element-type '(unsigned-byte 8))
     (let* ((len (file-length stream))
	    (data (make-array len :element-type '(unsigned-byte 8))))
       (dotimes (n len)
	 (setf (aref data n) (read-byte stream)))
       data)))

#+nil
(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))


#+nil
(defun getapixel (h w image)
  (destructuring-bind (height width c) (array-dimensions image)
    (declare (ignore height))
    (make-array 4 :element-type (array-element-type image)
		:displaced-to image
		:displaced-index-offset (* c (+ w (* h width))))))

#+nil
(defun complex-modulus (c)
  (sqrt (realpart (* c (conjugate c)))))
