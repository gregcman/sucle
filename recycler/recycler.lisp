(in-package :recycler)

;;next up is memory pooling
;;the pool is a data structure which holds things that can be reused
;;such as (simple-array (unsigned-byte 8) (4096))
;;and (simple-array (unsigned-byte 4) (4096))
;;if the pool is empty, it will make another object to give
;;1. need an array, ask pool for space
;;2. (if (pool empty) (pool makes another) ())
;;3. pool gives u array
(defstruct recycler
  create-func
  cleanup-func
  (pool-size 0)
  (pool nil)
  size-cap)
(defun get-from (recycler &rest specs)
  (if (recycler-pool recycler)
      (progn
	(decf (recycler-pool-size recycler))
	(let ((new-item (pop (recycler-pool recycler))))
	  (apply (recycler-cleanup-func recycler) new-item specs)))
      (apply (recycler-create-func recycler) specs)))
(defun give-to (recycler item &rest specs)
  (apply (recycler-cleanup-func recycler) item specs)
  (when (< (recycler-pool-size recycler) (recycler-size-cap recycler))
    (push item (recycler-pool recycler))
    (incf (recycler-pool-size recycler))))
