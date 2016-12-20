(in-package :sandbox)

(defun genhash ()
  (make-hash-table :test (function eql)))

;;chunkhash stores all of the chunks in a hashmap.
;;chunks accessed by '(x y z) in chunk coords
(defparameter chunkhash (genhash))
(defparameter lighthash (genhash))
(defparameter skylighthash (genhash))
(defparameter metahash (genhash))

(defparameter heighthash (genhash))

(defparameter daytime 1.0)

;;dirty chunks is a list of modified chunks
;;we do not want anyone to see a raw list!
(defparameter dirtychunks nil)
(defun clean-dirty ()
  (setf dirtychunks (q::make-uniq-q)))
(defun dirty-pop ()
  (q::uniq-pop dirtychunks))
(defun dirty-push (item)
  (q::uniq-push item dirtychunks))

;;initialize the world
(defun world-init ()
  (clean-dirty))

(defun clearworld ()
  (vox::send-to-free-mem chunkhash)
  (vox::send-to-free-mem lighthash)
  (vox::send-to-free-mem skylighthash)
  (vox::send-to-free-mem metahash)
  (pix::send-to-free-mem heighthash)
  (clean-dirty))

;;look at all the repetition here!! its clear a macro is in order
;;vox needs to be cleaned up
(vox::prep-hash setblock getblock chunkhash 0)
(vox::prep-hash setlight getlight lighthash 0)
(vox::prep-hash skysetlight skygetlight skylighthash 15)
(vox::prep-hash setmeta getmeta metahash 0)

(setf (fdefinition 'getheight) (pix::func-get heighthash 0))
(setf (fdefinition 'setheight) (pix::func-set heighthash 0))
(defun (setf getheight) (new x y)
  (setheight x y new))

(defun block-dirtify (i j k)
  (dirty-push (vox::chop (vox::chunkhashfunc i j k))))

(defun update-height (x y)
  (block wow
    (dorange (z 0 255)
	     (let ((val (- 255 z)))
	       (let ((the-block (getblock x val y)))
		 (let ((ans 
			 (eq t 
			      (aref mc-blocks::opaquecubelooukup the-block))))
		   (if ans
		       (return-from wow
			 (setf (getheight x y) val)))))))
    (setheight x y 0)))

(defun setblock-with-update (i j k blockid)
  (if (/= blockid (getblock i j k))
   (let ((new-light-value (aref mc-blocks::lightvalue blockid))
	 (old-light-value (getlight i j k)))
     (when (setblock i j k blockid)
       (if (< new-light-value old-light-value)
	   (progn
	     (de-light-node i j k)))
       (setlight i j k new-light-value)
       (sky-de-light-node i j k)
       (unless (zerop new-light-value)
	 (light-node i j k))
       (block-dirtify i j k)))))
