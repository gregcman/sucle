(in-package :sandbox)

;;chunkhash stores all of the chunks in a hashmap.
;;chunks accessed by '(x y z) in chunk coords
(defparameter chunkhash nil)
(defparameter lighthash nil)
(defparameter skylighthash nil)
(defparameter metahash nil)

(defparameter heighthash (make-hash-table))

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

(defun send-to-free-mem (hash pool)
  (maphash
   (lambda (k v)
     (declare (ignore k))
     (recycle:give-to pool v))
   hash)
  (clrhash hash))

;;clearing a chunk, referencing data inside a chunk,
;;creating a new chunk
(defun clearchunk (achunk &optional (value 0))
  (dotimes (x (array-total-size achunk))
    (setf (row-major-aref achunk x) value))
  achunk)

(defmacro %new-chunk (type defaultval chopx chopy chopz)
  `(make-array (ash 1 (+ ,chopx ,chopy ,chopz))
	       :element-type ',type
	       :initial-element ,defaultval))

(defparameter freechunkmempool8
  (recycle:make-recycler
   :create-func #'(lambda (&optional (x 0)) (%new-chunk (unsigned-byte 8) x 4 4 4))
   :cleanup-func #'clearchunk
   :size-cap 128))

(defparameter freechunkmempool4
  (recycle:make-recycler
   :create-func #'(lambda (&optional (x 0)) (%new-chunk (unsigned-byte 4) x 4 4 4))
   :cleanup-func #'clearchunk
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

(defun system ()
  (let ((uniform-spec (coge:gen-spec)))
    (vox::layout uniform-spec 25 0 25 26 9 52)
    (vox::truncation uniform-spec 4 4 4)
    (vox::derived-parts uniform-spec)
    (vox::offset uniform-spec 0 0 0)
    (vox::names uniform-spec
		'vox::unhashfunc 'vox::chunkhashfunc
		'vox::chop 'vox::anti-chop 'vox::rem-flow 'vox::%%ref 'vox::add)
    uniform-spec))


(defun gen-holder (bits setter getter hash default creator)
  (let ((new (system)))
    (vox::field new `(simple-array (unsigned-byte ,bits) (4096)) hash default creator)
    (vox::access new getter setter)
    (vox::prep-hash new)))

;;look at all the repetition here!! its clear a macro is in order
;;vox needs to be cleaned up
(defun establish-system ()
  (progn
    (eval (print (vox::define-fixnum-ops (system))))
    (eval (print (list
		  'progn
		  (gen-holder 8 'setblock 'getblock 'chunkhash 0 '(recycle:get-from freechunkmempool8 0))
		  (gen-holder 4 'setlight 'getlight 'lighthash 0 '(recycle:get-from freechunkmempool4 0))
		  (gen-holder 4 'skysetlight 'skygetlight 'skylighthash 15 '(recycle:get-from freechunkmempool4 15))
		  (gen-holder 4 'setmeta 'getmeta 'metahash 0 '(recycle:get-from freechunkmempool4 0))))))
  (setf (fdefinition 'getheight) (pix::func-get heighthash 0))
  (setf (fdefinition 'setheight) (pix::func-set heighthash 0))
  (defun (setf getheight) (new x y)
    (setheight x y new)))

(defun setup-hashes ()
  (setf chunkhash (vox::genhash))
  (setf lighthash (vox::genhash))
  (setf skylighthash (vox::genhash))
  (setf metahash (vox::genhash)))

(setup-hashes)
(establish-system)

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
