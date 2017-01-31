(in-package :sandbox)

(defparameter mesher-thread nil)
(defun designatemeshing ()
  (unless (mesherthreadbusy)
    (if mesher-thread
	(getmeshersfinishedshit))
    (let ((achunk (dirty-pop)))
      (when achunk
	(giveworktomesherthread achunk)))))

(defun shape-list (verts len)   
  (declare (type fixnum len)
	   (type (simple-array single-float *) verts))
  (unless (zerop len)
    (let ((ourlist (gl:gen-lists 1))
	  (vertsize 6))
      (declare (type fixnum vertsize len))
      (gl:new-list ourlist :compile)
      (macrolet ((wow (num start)
		   `(%gl:vertex-attrib-4f ,num
					  (aref verts (+ base ,(+ start 0)))
					  (aref verts (+ base ,(+ start 1)))
					  (aref verts (+ base ,(+ start 2)))
					  (aref verts (+ base ,(+ start 3)))))
		 (wow2 (num start)
		   `(%gl:vertex-attrib-2f ,num
					  (aref verts (+ base ,(+ start 0)))
					  (aref verts (+ base ,(+ start 1)))))
		 (wow3 (num start)
		   `(%gl:vertex-attrib-3f ,num
					  (aref verts (+ base ,(+ start 0)))
					  (aref verts (+ base ,(+ start 1)))
					  (aref verts (+ base ,(+ start 2)))))
		 (wow1 (num start)
		   `(%gl:vertex-attrib-1f ,num
					  (aref verts (+ base ,start)))))
	(gl:with-primitives :quads
	  (BLOCK NIL
	    (LET ((base 0)
		  (end (the fixnum (* len vertsize))))
	      (DECLARE (TYPE UNSIGNED-BYTE end base))
	      (TAGBODY
		 (GO end)
	       rep
		 (TAGBODY
		    (wow1 8 5)
		    (wow2 2 3)
		    (wow3 0 0)
		    )
		 (PSETQ base (the fixnum (+ vertsize base)))
	       end
		 (UNLESS (>= base end) (GO rep))
		 (RETURN-FROM NIL (PROGN NIL)))))))
      (gl:end-list)
      ourlist)))

(defun getmeshersfinishedshit ()
  (multiple-value-bind (shape len coords) (sb-thread:join-thread mesher-thread)
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

(defun mesherthreadbusy ()
  (not (or (eq nil mesher-thread)
	   (not (bordeaux-threads:thread-alive-p mesher-thread)))))

(defun giveworktomesherThread (thechunk)
  (setf mesher-thread
	(bordeaux-threads:make-thread
	 (lambda ()
	   (sb-thread:return-from-thread
	    (chunk-shape thechunk))))))
