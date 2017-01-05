(in-package :glshader)

;;;opengl can only use one shaderprogram at once,
;;;so there is a global *shader-program* variable
(defparameter *shader-program* nil)

;;;check if the 
(defun use-program (ourprog)
  (unless (eql ourprog *shader-program*)
    (setq *shader-program* ourprog)
    (gl:use-program ourprog)))

;;;attribs is an alist with a string in the car representing an attribute
;;;and a number representing the location in the cdr
(defun make-shader-program-from-strings
    (vertex-shader-string fragment-shader-string attribs)
  "makes a shader program from strings. makes noises if something goes wrong"
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
      (gl:delete-shader vertexShader)
      (gl:delete-shader fragmentShader)
      shaderProgram)))

;;;various functions for setting uniforms 
(defun set-matrix (name matrix)
  (gl:uniform-matrix-4fv
   (gl:get-uniform-location *shader-program* name)
   matrix))

(defun set-int (name thenumber)
  (gl:uniformi
   (gl:get-uniform-location *shader-program* name)
   thenumber))

(defun set-vec4 (name thevec4)
  (gl:uniformfv
   (gl:get-uniform-location *shader-program* name)
   thevec4))

(defun set-vec3 (name thevec3)
  (gl:uniformfv
   (gl:get-uniform-location *shader-program* name)
   thevec3))

(defun set-float (name thefloat)
  (gl:uniformf
   (gl:get-uniform-location *shader-program* name)
   thefloat))

