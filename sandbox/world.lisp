(in-package :sandbox)

(defun genhash ()
  (make-hash-table :test (function eql)))

;;chunkhash stores all of the chunks in a hasmap.
;;chunks accessed by '(x y z) in chunk coords
(defparameter chunkhash (genhash))
(defparameter lighthash (genhash))
(defparameter skylighthash (genhash))
(defparameter metahash (genhash))

(defparameter heighthash (genhash))

(defparameter daytime 1.0)

;;dirty chunks is a list of modified chunks 
(defparameter dirtychunks nil)

(defun clearworld ()
  (vox::send-to-free-mem chunkhash)
  (vox::send-to-free-mem lighthash)
  (vox::send-to-free-mem skylighthash)
  (vox::send-to-free-mem metahash)
  (pix::send-to-free-mem heighthash)
  (setf dirtychunks nil))

(setf (fdefinition 'getblock) (vox::func-get chunkhash 0))
(setf (fdefinition 'setblock) (vox::func-set chunkhash 0))
(defun (setf getblock) (new x y z)
    (setblock x y z new))

(setf (fdefinition 'getlight) (vox::func-get lighthash 0))
(setf (fdefinition 'setlight) (vox::func-set lighthash 0))
(defun (setf getlight) (new x y z)
    (setlight x y z new))

(setf (fdefinition 'skygetlight) (vox::func-get skylighthash 15))
(setf (fdefinition 'skysetlight) (vox::func-set skylighthash 15))
(defun (setf skygetlight) (new x y z)
    (skysetlight x y z new))

(setf (fdefinition 'getmeta) (vox::func-get metahash 0))
(setf (fdefinition 'setmeta) (vox::func-set metahash 0))
(defun (setf getmeta) (new x y z)
    (setmeta x y z new))

(setf (fdefinition 'getheight) (pix::func-get heighthash 0))
(setf (fdefinition 'setheight) (pix::func-set heighthash 0))
(defun (setf getheight) (new x y)
    (setheight x y new))

(defun block-dirtify (i j k)
  (pushnew (list (ash i -4) (ash j -4) (ash k -4)) dirtychunks :test 'equal))

(defun dirtify (x y z)
  (pushnew (list x y z) dirtychunks :test 'equal))

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
      (block-dirtify i j k))))

(defun round-pos (x y z)
  (getblock
   (round x)
   (round y)
   (round z)))
