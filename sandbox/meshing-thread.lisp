(in-package :sandbox)

(defparameter mesher-thread nil)
(defun designatemeshing ()
  (unless (mesherthreadbusy)
    (if mesher-thread
	(getmeshersfinishedshit))
    (let ((achunk (dirty-pop)))
      (when achunk
	(giveworktomesherthread achunk)))))

(defun shape-list (the-shape)
  (let ((ourlist (gl:gen-lists 1))
	(verts (shape-vs the-shape))
	(vertsize 17))
    (gl:new-list ourlist :compile)
    (macrolet ((wow (num start)
		 `(gl:vertex-attrib ,num
				    (elt verts (+ base ,start 0))
				    (elt verts (+ base ,start 1))
				    (elt verts (+ base ,start 2))
				    (elt verts (+ base ,start 3))))
	       (wow2 (num start)
		 `(gl:vertex-attrib ,num
				    (elt verts (+ base ,start 0))
				    (elt verts (+ base ,start 1))))
	       (wow3 (num start)
		 `(gl:vertex-attrib ,num
				    (elt verts (+ base ,start 0))
				    (elt verts (+ base ,start 1))
				    (elt verts (+ base ,start 2)))))
      (gl:with-primitives :quads
	(dotimes (x (shape-vertlength the-shape))
	  (let ((base (* x vertsize)))
	    (wow2 2 3)
	    (wow 3 5)
	    (wow 8 9)
	    (wow 12 13)
	    (wow3 0 0)))))
    (gl:end-list)
    ourlist))

(defun getmeshersfinishedshit ()
  (multiple-value-bind (coords shape) (sb-thread:join-thread mesher-thread)
    (if coords
	(if shape
	    (progn
	      (setf (gethash coords vaohash) (shape-list shape))
	      (setf worldlist (genworldcallist)))
	    (dirty-push coords))))
  (setf mesher-thread nil))

(defun mesherthreadbusy ()
  (not (or (eq nil mesher-thread)
	   (not (sb-thread:thread-alive-p mesher-thread)))))

(defun giveworktomesherThread (thechunk)
  (setf mesher-thread
	(sb-thread:make-thread
	 (lambda (chunk-position)
	   (sb-thread:return-from-thread
	    (values
	     chunk-position
	     (chunk-shape chunk-position))))
	 :arguments (list thechunk))))
