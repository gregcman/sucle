(in-package :sandbox)

;;chunkhash stores all of the chunks in a hashmap.
;;chunks accessed by '(x y z) in chunk coords
(defparameter chunkhash (vox::genhash))
(defparameter lighthash (vox::genhash))
(defparameter skylighthash (vox::genhash))
(defparameter metahash (vox::genhash))

(defparameter heighthash (make-hash-table))

(defparameter daytime 1.0)
(vox::define-fixnum-ops 0 25 4
			26 25 4
			52 9 4)
;;dirty chunks is a list of modified chunks
;;we do not want anyone to see a raw list!
(defparameter dirtychunks nil)
(defun clean-dirty ()
  (setf dirtychunks (q::make-uniq-q)))
(defun dirty-pop ()
  (q::uniq-pop dirtychunks))
(defun dirty-push (item)
  (q::uniq-push item dirtychunks))

(defun send-to-free-mem (hash pool)
  (maphash
   (lambda (k v)
     (declare (ignore k))
     (vox::give-to pool v))
   hash)
  (clrhash hash))

(defparameter freechunkmempool8
  (vox::make-provider
   :create-func #'(lambda (&optional (x 0)) (vox::%new-chunk (unsigned-byte 8) x 4 4 4))
   :cleanup-func #'vox::clearchunk
   :size-cap 128))

(defparameter freechunkmempool4
  (vox::make-provider
   :create-func #'(lambda (&optional (x 0)) (vox::%new-chunk (unsigned-byte 4) x 4 4 4))
   :cleanup-func #'vox::clearchunk
   :size-cap 512))

;;initialize the world
(defun world-init ()
  (clean-dirty))

(defun clearworld ()
  (send-to-free-mem chunkhash freechunkmempool8)
  (send-to-free-mem lighthash freechunkmempool4)
  (send-to-free-mem skylighthash freechunkmempool4)
  (send-to-free-mem metahash freechunkmempool4)
  (if nil
      (send-to-free-mem heighthash))
  (clean-dirty))

;;look at all the repetition here!! its clear a macro is in order
;;vox needs to be cleaned up
(progn
  (vox::prep-hash 8 setblock getblock chunkhash 0 (vox::get-from freechunkmempool8 0))
  (vox::prep-hash 4 setlight getlight lighthash 0 (vox::get-from freechunkmempool4 0))
  (vox::prep-hash 4 skysetlight skygetlight skylighthash 15 (vox::get-from freechunkmempool4 15))
  (vox::prep-hash 4 setmeta getmeta metahash 0 (vox::get-from freechunkmempool4 0)))

(setf (fdefinition 'getheight) (pix::func-get heighthash 0))
(setf (fdefinition 'setheight) (pix::func-set heighthash 0))
(defun (setf getheight) (new x y)
  (setheight x y new))

(defun block-dirtify (i j k)
  (dirty-push (vox::chop (vox::chunkhashfunc i k j))))

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
