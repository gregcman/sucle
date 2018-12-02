(defpackage #:glhelp
  (:use #:cl))

(in-package :glhelp)

(defparameter *gl-version* "2.0") ;;abitrary, gets overwritten

;;;;in opengl horizontal lines must be multiples of 4 bytes
(defun array-flatten (array)
  (make-array (array-total-size array)
	      :displaced-to array
	      :element-type (array-element-type array)))

(defun create-texture (tex-data width height format &optional (type :unsigned-byte))
  (let ((tex (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d tex)
    (gl:tex-image-2d :texture-2d 0 :rgba width height 0 format type tex-data)
    tex))


(defparameter *default-tex-params* (quote ((:texture-min-filter . :nearest)
					   (:texture-mag-filter . :nearest)
;					   (:texture-wrap-s . :repeat)
;					   (:texture-wrap-t . :repeat)
					   )))

;;;;tex-parameters is an alist of pairs (a . b) with
;;;;(lambda (a b) (gl:tex-parameter :texture-2d a b))
(defun pic-texture (thepic &optional type)
  (let ((dims (array-dimensions thepic)))
    (let ((h (pop dims))
	  (w (pop dims))
	  (channels (pop dims)))
      (unless type
	(setf
	 type
	 (case channels
	   ((nil) :red)
	   (2 (error "2 components?"))
	   (3 :rgb)
	   (4 :rgba))))
      (when (eq nil channels)
	(setf channels 1))
      (let* ((byte-width (* channels w))
	     (foured (ash (ash byte-width -2) 2)))
	(if (= byte-width foured)
	    (create-texture (array-flatten thepic) w h type)
	    (progn
	      (incf foured 4)
	      (let ((array (make-array (* foured h)
				       :element-type (array-element-type thepic))))
;		(declare (dynamic-extent array))
		(dotimes (hi h)
		  (let ((base1 (* hi byte-width))
			(base2 (* hi foured)))
		    (dotimes (wi byte-width)
		      (setf (aref array (+ wi base2))
			    (row-major-aref thepic (+ wi base1))))))
		(create-texture array w h type))))))))

(export '(apply-tex-params))
(defun apply-tex-params (tex-parameters)
  (dolist (param tex-parameters)
    (gl:tex-parameter :texture-2d (car param) (cdr param))))

(defun compile-string-into-shader (shader string)
  (gl:shader-source shader string)
  (gl:compile-shader shader)
  (let ((success (gl:get-shader-info-log shader)))
    (unless (gl-success success)
      (print success)
      (error "~S" success))))

(defun string-match? (a b)
  (let ((log (remove "" (split-sequence:split-sequence #\Newline a)
		     :test #'string=)))
    (string= (car (last log))
	     b)))

(defun gl-success (success)
  (or (zerop (length success))
      (string-match? success "No errors.")))

;;;;attribs is an alist of pairs (a . b) with
;;;;(lambda (a b) (gl:bind-attrib-location ...[the program].. a b)
(defun make-shader-program-from-strings (vs-string fs-string attribs)
  (let ((vert (gl:create-shader :vertex-shader))
	(frag (gl:create-shader :fragment-shader))
	(program (gl:create-program)))

    (compile-string-into-shader frag fs-string)
    (compile-string-into-shader vert vs-string)
    
    (gl:attach-shader program vert)
    (gl:attach-shader program frag)

    (dolist (val attribs)
      (gl:bind-attrib-location program
			       (cdr val)
			       (car val)))
    
    (gl:link-program program)
    (let ((success (gl:get-program-info-log program)))
      (unless (gl-success success)
	(print success)
	(error "~S" success)))
    (gl:detach-shader program vert)
    (gl:detach-shader program frag)
    
    (gl:delete-shader vert)
    (gl:delete-shader frag)
    program))

(export (quote (pic-texture make-shader-program-from-strings)))


(defun sizeof (type-keyword)
  "gets the size of a foreign c type"
  (cffi:foreign-type-size type-keyword))

(defun get-gl-constant (keyword)
  "gets a gl-constant"
  (cffi:foreign-enum-value '%gl:enum keyword))

(progn
  (defconstant +gltexture0+ (cffi:foreign-enum-value (quote %gl:enum) :texture0))
  (defun set-active-texture (num)
    (gl:active-texture (+ num +gltexture0+))))

(defun bind-default-framebuffer ()
  (gl:bind-framebuffer :framebuffer 0))


(defun create-framebuffer (w h)
  (let ((framebuffer (first (gl:gen-framebuffers 1)))
        (depthbuffer (first (gl:gen-renderbuffers 1)))
        (texture (first (gl:gen-textures 1))))
    ;; setup framebuffer
    (gl:bind-framebuffer :framebuffer framebuffer)

    (progn
      ;; setup texture and attach it to the framebuffer
      (gl:bind-texture :texture-2d texture)
      (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
      (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
      (gl:tex-image-2d :texture-2d 0 :rgba w h 0 :rgba :unsigned-byte (cffi:null-pointer))
      (gl:bind-texture :texture-2d 0)
      (gl:framebuffer-texture-2d :framebuffer
				 :color-attachment0
				 :texture-2d
				 texture
				 0))
    (progn
      ;; setup depth-buffer and attach it to the framebuffer
      (gl:bind-renderbuffer :renderbuffer depthbuffer)
      (gl:renderbuffer-storage :renderbuffer :depth-component24 w h)
      (gl:framebuffer-renderbuffer
       :framebuffer
       :depth-attachment
       :renderbuffer
       depthbuffer))

    ;; validate framebuffer
    (let ((framebuffer-status (gl:check-framebuffer-status :framebuffer)))
      (unless (gl::enum= framebuffer-status :framebuffer-complete)
        (error "Framebuffer not complete: ~A." framebuffer-status)))

    #+nil
    (gl:clear-color 0.0 0.0 0.0 0.0)
    #+nil
    (gl:clear :color-buffer-bit
	      :depth-buffer-bit)
    #+nil
    (gl:enable :depth-test ;:multisample
	       )
    (values texture framebuffer depthbuffer)))

(defun glsl-gl-version (&optional (version *gl-version*))
  (let ((string (subseq version 0 3)))
    (second
     (assoc
      string
      (quote (("2.0" 110)
	      ("2.1" 120)
	      ("3.0" 130)
	      ("3.1" 140)
	      ("3.2" 150)
	      ("3.3" 330)
	      ("4.0" 400)
	      ("4.1" 410)
	      ("4.2" 420)
	      ("4.3" 430)
	      ("4.4" 440)
	      ("4.5" 450)
	      ("4.6" 460)))
      :test 'string=))))
