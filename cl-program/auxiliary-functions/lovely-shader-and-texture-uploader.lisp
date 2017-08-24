(defpackage :lovely-shader-and-texture-uploader
  (:use :cl))

(in-package :lovely-shader-and-texture-uploader)


(defun array-flatten (array)
  (make-array (array-total-size array)
	      :displaced-to array
	      :element-type (array-element-type array)))

(defun create-texture (tex-data width height format &optional (type :unsigned-byte))
  (let ((tex (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d tex)
    (gl:tex-image-2d :texture-2d 0 format width height 0 format type tex-data)
    tex))


(defparameter *default-tex-params* (quote ((:texture-min-filter . :nearest)
					   (:texture-mag-filter . :nearest)
					   (:texture-wrap-s . :repeat)
					   (:texture-wrap-t . :repeat))))
;;;;tex-parameters is an alist of pairs (a . b) with
;;;;(lambda (a b) (gl:tex-parameter :texture-2d a b))
(defun pic-texture (thepic type)
  (let ((dims (array-dimensions thepic)))
    (let ((h (pop dims))
	  (w (pop dims)))
      (let ((texture (create-texture (array-flatten thepic) w h type)))
	texture))))

(defun apply-tex-params (tex-parameters)
  (dolist (param tex-parameters)
    (gl:tex-parameter :texture-2d (car param) (cdr param))))

(defun compile-string-into-shader (shader string)
  (gl:shader-source shader string)
  (gl:compile-shader shader)
  (let ((success (gl:get-shader-info-log shader)))
    (unless (zerop (length success))
      (print success)
      (error "~S" success))))

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
      (unless (zerop (length success))
	(print success)
	(error "~S" success)))
    (gl:detach-shader program vert)
    (gl:detach-shader program frag)
    
    (gl:delete-shader vert)
    (gl:delete-shader frag)
    program))

(export (quote (pic-texture make-shader-program-from-strings)))
