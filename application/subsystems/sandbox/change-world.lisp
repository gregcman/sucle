(in-package :sandbox)

(defun draw-world ()
  (declare (optimize (speed 3) (safety 0)))
  ;(let ((a (get-internal-real-time))))
  (dohash (key value) *g/chunk-call-list*
	  (declare (ignore key))
	  (gl:call-list value))
  ;(print (- (get-internal-real-time) a))
  )

(progn
  (defparameter *g/chunk-call-list* (make-hash-table :test 'eq))
  (defun get-chunk-display-list (name)
    (gethash name *g/chunk-call-list*))
  (defun set-chunk-display-list (name list-num)
    (setf (gethash name *g/chunk-call-list*) list-num))
  (defun remove-chunk-display-list (name)
    (remhash name *g/chunk-call-list*)))

(defun update-world-vao (x y z)
  (clean-dirty)
  (lparallel:kill-tasks 'mesh-chunk)
  (maphash (lambda (k v)
	     (declare (ignorable k))
	     (gl:delete-lists v 1)
	     (remove-chunk-display-list k))
	   *g/chunk-call-list*)
  (map nil #'dirty-push
       (sort (alexandria:hash-table-keys world::*lispobj*) #'< :key
	     (lambda (position)
	       (multiple-value-bind (i j k) (world:unhashfunc position)
		 ((lambda (x0 y0 z0 x1 y1 z1)
		    (let ((dx (- x1 x0))
			  (dy (- y1 y0))
			  (dz (- z1 z0)))
		      (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
		  x y z
		  (- i 8)
		  (- j 8)
		  (- k 8)))))))

(defun update-chunk-mesh (coords iter)
  (when coords
    (let ((old-call-list (get-chunk-display-list coords)))     
      (when old-call-list (gl:delete-lists old-call-list 1)))
    (with-vec (a b c) (iter)
      (let ((len (floor (scratch-buffer:iterator-fill-pointer a) 3)))
	(if (zerop len)
	    (remove-chunk-display-list coords)	  
	    (set-chunk-display-list
	     coords
	     (glhelp:with-gl-list
	       (gl:with-primitives :quads	     
		 (scratch-buffer:flush-my-iterator a
		   (scratch-buffer:flush-my-iterator b
		     (scratch-buffer:flush-my-iterator c
		       (mesh-chunk len a b c))))))))))))

(defun mesh-chunk (times a b c)
  (declare (type fixnum times))
  (declare (optimize (speed 3) (safety 0)))
  (bind-iterator-in
   (xyz single-float) a
   (bind-iterator-in
    (uv single-float) b
    (bind-iterator-in
     (dark single-float) c
     (dotimes (x times)
       (%gl:vertex-attrib-3f 2 (xyz) (xyz) (xyz))
       (%gl:vertex-attrib-2f 8 (uv) (uv))
       (%gl:vertex-attrib-4f 1 (dark) (dark) (dark) (dark))
       (%gl:vertex-attrib-4f 0 (dark) (dark) (dark) (dark));;;zero always comes last?
       )))))

(defun attrib-buffer-iterators ()
  (map-into (make-array 3 :element-type t :initial-element nil)
	    (function scratch-buffer:my-iterator)))
(defparameter *achannel* (lparallel:make-channel))
(defun designatemeshing ()
  (loop
     (multiple-value-bind (value success-p) (lparallel:try-receive-result *achannel*)
       (if success-p
	   (apply (car value) (cdr value))
	   (return))))
  (loop
     (let ((thechunk (dirty-pop)))
       (if thechunk
	   (let ((lparallel:*task-category* 'mesh-chunk))
	     (lparallel:submit-task
	      *achannel*
	      (lambda (iter space chunk-pos)
		(map nil (lambda (x) (scratch-buffer:free-my-iterator-memory x)) iter)
		(multiple-value-bind (io jo ko) (world:unhashfunc chunk-pos)
		  (chunk-shape iter io jo ko)
		  (%list space #'update-chunk-mesh chunk-pos iter)))
	      (attrib-buffer-iterators)
	      (make-list 3)
	      thechunk))
	   (return)))))


(defun setblock-with-update (i j k blockid &optional
					     (new-light-value (aref block-data:*lightvalue* blockid)))
 (let ((old-light-value (world:getlight i j k)))
    (when (setf (world:getblock i j k) blockid)
      (when (< new-light-value old-light-value)
	(de-light-node i j k))
      (unless (= old-light-value new-light-value)
	(setf (world:getlight i j k) new-light-value))
      (sky-de-light-node i j k)
      (unless (zerop new-light-value)
	(light-node i j k))
      (flet ((check (a b c)
	       (light-node (+ i a) (+ j b) (+ k c))
	       (sky-light-node (+ i a) (+ j b) (+ k c))))
	(check -1 0 0)
	(check 1 0 0)
	(check 0 -1 0)
	(check 0 1 0)
	(check 0 0 -1)
	(check 0 0 1))
      (block-dirtify i j k))))

(defun plain-setblock (i j k blockid &optional
				       (new-light-value (aref block-data:*lightvalue* blockid))
				       (new-sky-light-value 0))
  (when (setf (world:getblock i j k) blockid)
    (setf (world:getlight i j k) new-light-value)
    (setf (world:skygetlight i j k) new-sky-light-value)
    (block-dirtify i j k)))

#+nil
(defparameter *save*
  '("#version 100
precision lowp float;
attribute vec4 position;
attribute vec2 texCoord;
attribute float darkness;
uniform mat4 projectionmodelview;
varying vec2 TexCoord;
varying float mycolor;
void main(){
gl_Position = projectionmodelview * position;
mycolor = darkness;
TexCoord = texCoord;
}
"
    "#version 100
precision lowp float;
varying vec2 TexCoord;
varying float mycolor;
uniform sampler2D ourTexture;
void main(){
vec4 texcolor = texture2D(ourTexture, TexCoord);
gl_FragColor.rgb = mycolor * texcolor.rgb;
}
") )
