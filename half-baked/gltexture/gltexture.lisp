(in-package :gltexture)

(defun glActiveTexture (num)
  "sets the active texture"
  (gl:active-texture (+ num (glinfo:get-gl-constant :texture0))))

(defun create-texture (tex-data width height &optional (type :rgba))
  "creates an opengl texture from data"
  (let ((the-shit (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d the-shit)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest; :nearest-mipmap-nearest
		      )
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:tex-parameter :texture-2d :texture-border-color '(0 0 0 0))
   ; (gl:tex-parameter :texture-2d :generate-mipmap :true)
    (gl:tex-image-2d
     :texture-2d 0
     type width height 0 type :unsigned-byte tex-data)
 ;   (gl:generate-mipmap :texture-2d)
    the-shit))

