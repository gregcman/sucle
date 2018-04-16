(defpackage :sprite-chain
  (:use #:cl)
  (:export
   #:remove-sprite
   #:add-sprite
   #:topify-sprite
   #:do-sprite-chain))
(in-package :sprite-chain)

(struct-to-clos:struct->class
 (defstruct sprite-chain
   (chain (doubly-linked-list:circular "sentinel"))
   (hash (make-hash-table :test 'eq))))
(defparameter *sprites* (make-sprite-chain))

(defun add-sprite (sprite &optional (sprite-chain *sprites*))
  (let ((hash (sprite-chain-hash sprite-chain)))
    (multiple-value-bind (old-cell exists-p)
	(gethash sprite hash)
      (declare (ignorable old-cell))
      (unless exists-p
	(let ((new-node (doubly-linked-list:make-node
			 :payload
			 sprite)))
	  (setf (gethash sprite hash)
		new-node)
	  (doubly-linked-list:insert-next
	   (sprite-chain-chain sprite-chain)
	   new-node)))
      (not exists-p))))

(defun remove-sprite (sprite &optional (sprite-chain *sprites*))
  (let ((hash (sprite-chain-hash sprite-chain)))
    (multiple-value-bind (old-cell exists-p) (gethash sprite hash)
      (when exists-p
	(doubly-linked-list:detach old-cell)
	(remhash sprite hash))
      exists-p)))

(defun topify-sprite (sprite &optional (sprite-chain *sprites*))
  (let ((hash (sprite-chain-hash sprite-chain)))
    (multiple-value-bind (old-cell exists-p) (gethash sprite hash)
      (when exists-p
	(doubly-linked-list:detach old-cell)
	(doubly-linked-list:insert-next
	 (sprite-chain-chain sprite-chain)
	 old-cell))
      exists-p)))

(defmacro do-sprite-chain ((var &optional (backwards-p nil))
				  (&optional (chain '*sprites*))
			   &body body)
  `(doubly-linked-list:do-circular-doubly-linked-list (,var ,(not backwards-p))
       (sprite-chain-chain ,chain)
     ,@body))
