(in-package :sandbox)
(defparameter shaderProgram nil)
(defparameter shaderhash (make-hash-table :test #'equal))

(defun set-matrix (name matrix)
  (gl:uniform-matrix-4fv
   (gl:get-uniform-location shaderProgram name)
   (mat:to-flat matrix)))

(defun set-int (name thenumber)
  (gl:uniformi
   (gl:get-uniform-location shaderProgram name)
   thenumber))

(defun set-vec4 (name thevec4)
  (gl:uniformfv
   (gl:get-uniform-location shaderProgram name)
   thevec4))

(defun set-vec3 (name thevec3)
  (gl:uniformfv
   (gl:get-uniform-location shaderProgram name)
   thevec3))

(defun set-float (name thefloat)
  (gl:uniformf
   (gl:get-uniform-location shaderProgram name)
   thefloat))

(defun load-a-shader (name vs frag attribs)
  (setf (gethash name shaderhash)
	(load-and-make-shader
	 vs
	 frag
	 attribs)))

(defun load-and-make-shader (vpath fpath attribs)
  "loads a shader from a filepath and puts it into a program"
  (make-shader-program-from-strings
   (load-shader-file vpath)
   (load-shader-file fpath)
   attribs))

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

(defun use-program (name)
  (let ((ourprog (gethash name shaderhash)))
    (unless (eql ourprog shaderProgram)
      (setq shaderProgram ourprog)
      (gl:use-program ourprog))))

