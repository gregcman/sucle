(in-package :sandbox)

(defun pic-texture (thepic)
  (let ((dims (array-dimensions thepic)))
    (let ((h (pop dims))
	  (w (pop dims))) 
      (let ((type (case (car dims)
		    ((nil) :luminance)
		    (3 :rgb)
		    (4 :rgba))))
	(let ((new-texture (create-texture (array-flatten thepic) w h type)))
	  new-texture)))))

;;;turn a multidimensional array into a single dimensional array
;;;of the same total length
(defun array-flatten (array)
  (make-array (array-total-size array)
	      :displaced-to array
	      :element-type (array-element-type array)))


(defun create-texture (tex-data width height &optional (type :rgba))
  (let ((the-shit (gl:gen-texture)))
    (gl:bind-texture :texture-2d the-shit)
    (gl:tex-image-2d :texture-2d 0 type width height 0 type :unsigned-byte tex-data)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    the-shit))

;;;attribs is an alist with a string in the car representing an attribute
;;;and a number representing the location in the cdr
(defun make-shader-program-from-strings
    (vertex-shader-string fragment-shader-string attribs)
  (block nil
    (let ((vertexShader (gl:create-shader :vertex-shader))
	  (fragmentShader (gl:create-shader :fragment-shader))
	  (shaderProgram (gl:create-program)))
      (dolist (val attribs)
	(gl:bind-attrib-location shaderProgram
				 (cdr val)
				 (car val)))
      (gl:shader-source vertexShader vertex-shader-string)
      (gl:compile-shader vertexShader)
      (let ((success (gl:get-shader-info-log vertexShader)))
	(unless (zerop (length success))
	  (return (print success))))
      (gl:shader-source fragmentShader fragment-shader-string)
      (gl:compile-shader fragmentShader)
      (let ((success (gl:get-shader-info-log fragmentShader)))
	(unless (zerop (length success))
	  (return (print success))))
      (gl:attach-shader shaderProgram vertexShader)
      (gl:attach-shader shaderProgram fragmentShader)
      (gl:link-program shaderProgram)
      (let ((success (gl:get-program-info-log shaderProgram)))
	(unless (zerop (length success))
	  (return (print success))))
      (gl:detach-shader shaderProgram vertexshader)
      (gl:detach-shader shaderProgram fragmentshader)
      (gl:delete-shader vertexShader)
      (gl:delete-shader fragmentShader)
      shaderProgram)))
