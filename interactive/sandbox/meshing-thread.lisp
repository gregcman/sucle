(in-package :sandbox)

(defun mesh-chunk (times a b c)
  (declare (type fixnum times))
  (declare (optimize (speed 3) (safety 0)))
  (iter-ator:bind-iterator-in
   (xyz single-float) a
   (iter-ator:bind-iterator-in
    (uv single-float) b
    (iter-ator:bind-iterator-in
     (dark single-float) c
     (dotimes (x times)
       
       (%gl:vertex-attrib-1f 8 (dark))
       (%gl:vertex-attrib-2f 2 (uv) (uv))
       (%gl:vertex-attrib-3f 0 (xyz) (xyz) (xyz)))))))

(defmacro with-gl-list (&body body)
  (let ((list-sym (gensym)))
    `(let ((,list-sym (gl:gen-lists 1)))
       (gl:new-list ,list-sym :compile)
       ,@body
       (gl:end-list)
       ,list-sym)))

(defun update-chunk-mesh (len coords iter)
  (when coords
    (setf *chunks-changed* t)
    (let ((old-call-list (get-chunk-display-list coords)))     
      (when old-call-list (gl:delete-lists old-call-list 1)))
    (if (zerop len)
	(remove-chunk-display-list coords)	  
	(set-chunk-display-list
	 coords
	 (with-gl-list
	   (gl:with-primitives :quads
	     
	     (with-vec (a b c) (iter)
	       (flush-my-iterator a
		 (flush-my-iterator b
		   (flush-my-iterator c
		     (mesh-chunk len a b c)))))))))))

(defparameter *attrib-buffer-iterators*
  (map-into (make-array 3 :element-type t :initial-element nil)
	    (lambda ()
	      (my-iterator))))

(defparameter mesher-thread nil)
(defun designatemeshing ()
  (when (or (eq nil mesher-thread)
	    (not (bordeaux-threads:thread-alive-p mesher-thread)))
    (when mesher-thread
      (multiple-value-call #'update-chunk-mesh (sb-thread:join-thread mesher-thread)) 
      (setf mesher-thread nil))
    (let ((thechunk (dirty-pop)))
      (when thechunk
	(setf mesher-thread
	      (bordeaux-threads:make-thread
	       (lambda ()
		 (let ((iter *attrib-buffer-iterators*))
		   (map nil (lambda (x) (free-my-iterator-memory x)) iter)
		   (sb-thread:return-from-thread
		    (chunk-shape thechunk iter))))))))))
#+nil
(defun getmeshersfinishedshit ()
  (multiple-value-bind (shape len coords) (sb-thread:join-thread mesher-thread)
    ;;   (incf *faces* len)
    (when coords
      (when shape
	(let ((old-call-list (get-chunk-display-list coords)))
	  (let ((new (shape-list shape len)))
	    (if new
		(set-chunk-display-list coords new)
		(remove-chunk-display-list coords)))
	  (when old-call-list (gl:delete-lists old-call-list 1))
	  (let ((old-world (get-display-list :world)))
	    (remove-display-list :world)
	    (when old-world (gl:delete-lists old-world 1)))))))
  (setf mesher-thread nil))
