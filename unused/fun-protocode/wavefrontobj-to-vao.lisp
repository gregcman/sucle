 
  #+nil
  (when *sandbox-on*
       (initbag)
    )

	    #+nil
	    (progn
	      (gl:bind-texture
	       :texture-2d
	       (aplayground::get-stuff
		:lady
		aplayground::*stuff*
		aplayground::*backup*
		
		))
	      (fuck::draw-baggins))

(defclass vao ()
  ((vbuff :accessor vertex-buffer)
   (ibuff :accessor index-buffer)
   (va :accessor vertex-array)))

(defparameter *yolobaggins* (make-instance 'vao))

(defun reset-bag ()
  (let ((w *yolobaggins*))
    (gl:delete-vertex-arrays (list (vertex-array w)))
    (gl:delete-buffers (list (vertex-buffer w) (index-buffer w))))
  (initbag))

(defun initbag ()
  (aplayground::bornfnc
     :lady-png
     (lambda ()
       (aplayground::flip-image
	(aplayground::load-png 
	 "/home/imac/Documents/stuff2/NightFox/nightfox_d_4.png"))))
    (aplayground::bornfnc
     :lady
     (lambda ()
       (prog1
	   (lovely-shader-and-texture-uploader:pic-texture
	    (aplayground::get-stuff :lady-png aplayground::*stuff*
				    aplayground::*backup*)
	    :rgb)
	 (lovely-shader-and-texture-uploader::apply-tex-params
	  (quote ((:texture-min-filter . :linear)
		  (:texture-mag-filter . :linear)
		  (:texture-wrap-s . :repeat)
		  (:texture-wrap-t . :repeat)))))))
    
    (mostuff *yolobaggins*))

(progno
    (bind-shit :lady)
    (fuck::draw-baggins))

(defun draw-baggins ()
  (let ((w *yolobaggins*))
  ;;  (gl:disable :cull-face :blend)
;;    (gl:polygon-mode :front-and-back :fill)
    (gl:bind-vertex-array (vertex-array w))
    
    ;; This call actually does the rendering. The vertex data comes from
    ;; the currently-bound VAO. If the input array is null, the indices
    ;; will be taken from the element array buffer bound in the current
    ;; VAO.
    (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int)
		      :count (* 3 (array-total-size (gethash "indices" woywoy)))
		      :offset 0)))

(defparameter woywoy (cl-mesh:parse-wavefront-obj "/home/imac/Documents/stuff2/NightFox/NightFox.obj"))



(defparameter unique-vertices (make-hash-table :test 'equalp))
(defun generate-vertex-hash ()
  (let* ((name 0)
	 (indexes (gethash "indices" woywoy))
	 (len (array-total-size indexes))
	 (hash unique-vertices))
    (flet ((ass (vec)
	     (let ((value (gethash vec hash)))
	       (if value
		   nil
		   (progn (setf (gethash vec hash) name)
			  (incf name))))))
      (dotimes (i len)
	(let ((vec (aref indexes i)))
	  (let ((a (aref vec 0))
		(b (aref vec 1))
		(c (aref vec 2)))

	    (ass a)
	    (ass b)
	    (ass c)))))))
(defparameter vertarray nil)
(defun order-vertices ()
  (let ((hash unique-vertices))
    (let ((array (make-array (hash-table-count hash))))
      (maphash (lambda (k v)
		 (setf (aref array v) k))
	       hash)
      (setf vertarray array))))
(defparameter vertbuf nil)
(defun flatten-vert ()
  (let* ((array vertarray)
	 (len (array-total-size array))
	 (stride 6))
    (let ((buf (make-array (* len stride))))
      (let ((verts (gethash "vertices" woywoy))
	    (uv (gethash "uv" woywoy)))
	(dotimes (index len)
	  (let ((vec (aref array index)))
	    (let ((base (* index stride)))
	      (let ((verts (aref verts (aref vec 0)))
		    (uv (aref uv (aref vec 1))))
		(setf (aref buf (+ base 0)) (aref verts 0))
		(setf (aref buf (+ base 1)) (aref verts 1))
		(setf (aref buf (+ base 2)) (aref verts 2))
		(setf (aref buf (+ base 3)) 1.0)
		(setf (aref buf (+ base 4)) (aref uv 0))
		(setf (aref buf (+ base 5)) (aref uv 1))
		)))))
      (setf vertbuf buf))))

(progn (generate-vertex-hash)
       (order-vertices)
       (flatten-vert))


(defun mostuff (w)
  (let ((buffers (gl:gen-buffers 2)))
    (setf (vertex-buffer w) (elt buffers 0)
	  (index-buffer w) (elt buffers 1)))
  (gl:bind-buffer :array-buffer (vertex-buffer w))
  (let ((verts vertbuf))
    (let ((arr (gl:alloc-gl-array :float (length verts))))
      (dotimes (i (array-total-size verts))
	(setf (gl:glaref arr i) (aref verts i)))
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr)))
  ;; 0 is always reserved as an unbound object.
  (gl:bind-buffer :array-buffer 0)

  ;; An element array buffer stores vertex indices. We fill it in the
  ;; same way as an array buffer.
  (gl:bind-buffer :element-array-buffer (index-buffer w))
  (let ((hash unique-vertices))
    (let* ((indexes (gethash "indices" woywoy))
	   (len (array-total-size indexes)))
      (let ((arr (gl:alloc-gl-array :unsigned-int (* 3 len))))
	(dotimes (i len)
	  (let ((vec (aref indexes i))
		(base (* 3 i)))
	    (let ((a (aref vec 0))
		  (b (aref vec 1))
		  (c (aref vec 2)))
	      (setf (gl:glaref arr base) (gethash a hash))
	      (setf (gl:glaref arr (+ 1 base)) (gethash b hash))
	      (setf (gl:glaref arr (+ 2 base)) (gethash c hash)))))
	
	(gl:buffer-data :element-array-buffer :static-draw arr)
	(gl:free-gl-array arr))))
  (gl:bind-buffer :element-array-buffer 0)

  ;; Vertex array objects manage which vertex attributes are
  ;; associated with which data buffers. 
  (setf (vertex-array w) (gl:gen-vertex-array))
  (gl:bind-vertex-array (vertex-array w))

  ;; To associate our VBO data with this VAO, we bind it, specify
  ;; which vertex attribute we want to associate it with, and specify
  ;; where the data comes from.
  (gl:bind-buffer :array-buffer (vertex-buffer w))
  ;; In this program, we use attribute 0 for position. If you had
  ;; per-vertex normals, you could use a different attribute for those
  ;; as well.
  (gl:enable-vertex-attrib-array 0)
  ;; Using a null pointer as the data source indicates that we want
  ;; the vertex data to come from the currently bound array-buffer.
  (gl:vertex-attrib-pointer 0 4 :float nil (* 4 6) 0)

  (gl:enable-vertex-attrib-array 2)
  (gl:vertex-attrib-pointer 2 2 :float nil (* 4 6) (* 4 4))



  ;; To associate an element array with this VAO, all we need to do is
  ;; bind the element array buffer we want to use.
  (gl:bind-buffer :element-array-buffer (index-buffer w))

  ;; Once we're done, we can unbind the VAO, and rebind it when we want to render it.
  (gl:bind-vertex-array 0))
