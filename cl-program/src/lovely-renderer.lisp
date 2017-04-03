(in-package :sandbox)

(defun pic-texture (thepic type tex-parameters)
  (let ((dims (array-dimensions thepic)))
    (let ((h (pop dims))
	  (w (pop dims)))
      (let ((texture (create-texture (array-flatten thepic) w h type)))
	(dolist (param tex-parameters)
	  (gl:tex-parameter :texture-2d (car param) (cdr param)))
	texture))))

(defun array-flatten (array)
  (make-array (array-total-size array)
	      :displaced-to array
	      :element-type (array-element-type array)))


(defun create-texture (tex-data width height type)
  (let ((tex (car (glgentextures 1))))
    (gl:bind-texture :texture-2d tex)
    (gl:tex-image-2d :texture-2d 0 type width height 0 type :unsigned-byte tex-data)
    tex))

(defun make-shader-program-from-strings (vs-string fs-string attribs)
  (let ((vert (glcreateshader :vertex-shader))
	(frag (glcreateshader :fragment-shader))
	(program (glcreateprogram)))

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
    
    (gldeleteshader vert)
    (gldeleteshader frag)
    program))

(defun compile-string-into-shader (shader string)
  (gl:shader-source shader string)
  (gl:compile-shader shader)
  (let ((success (gl:get-shader-info-log shader)))
    (unless (zerop (length success))
      (print success)
      (error "~S" success))))
