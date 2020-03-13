(in-package :lem-sucle)
(defun empty-overlay-p (overlay)
  (lem:point=
   (lem:overlay-start overlay)
   (lem:overlay-end overlay)))

(defparameter *overlays* (make-hash-table :test 'eq))
(defun remove-overlays-of-buffer (buffer)
  (remhash buffer *overlays*))

(defun delete-all-overlays ()
  (utility:dohash (k v) *overlays*
    (declare (ignorable k))
    (mapc 'our-delete-overlay v)
    ;;FIXME::remove the 
    ))

(defun remove-empty-overlays-of-buffer (buffer)
  (let* ((data (gethash buffer *overlays*))
         (to-delete (remove-if-not 'empty-overlay-p
                                   data)))
    (when to-delete
      ;;FIXME::plural format string? formatter?
      (format *output* "~%deleting ~a overlays" (length to-delete))
      (mapc 'our-delete-overlay
            to-delete))))

(defun our-make-overlay (start end attribute)
  (let* ((overlay (lem:make-overlay start end attribute))
	 (buffer (lem:overlay-buffer overlay)))
    (unless (gethash buffer *overlays*)
      (lem:add-hook
       (lem:variable-value 'lem:kill-buffer-hook :buffer
                           buffer)
       'remove-overlays-of-buffer)
      (lem:add-hook
       (lem:variable-value 'lem:after-change-functions :buffer
                           buffer)
       'foo))
    (setf (lem-sucle::sucle-attribute-overlay attribute)
	  overlay)
    (push overlay (gethash buffer *overlays*))
    overlay))

(defparameter *output* *standard-output*)
(defun foo (&rest rest)
  (destructuring-bind (start end len) rest
    (declare (ignore end len))
    (let ((buffer (lem:point-buffer start)))
      (remove-empty-overlays-of-buffer buffer))
    ;;(print rest *output*)
    ))

(defun our-delete-overlay (overlay)
  (let ((buffer (lem:overlay-buffer overlay)))
    (lem:delete-overlay overlay)
    (let ((value (gethash buffer *overlays*)))
      (if value
          (progn
            (let ((new-list (delete overlay value)))
              (if new-list 
                  (setf (gethash buffer *overlays*)
                        new-list)
                  (progn
                    (remhash buffer *overlays*)
                    (lem:remove-hook
                     (lem:variable-value 'lem:kill-buffer-hook :buffer
                                         buffer)
                     'remove-overlays-of-buffer)
                    (lem:remove-hook
                     (lem:variable-value 'lem:after-change-functions :buffer
                                         buffer)
                     'foo))))
            t)
          nil))))

(defun copy-attribute-to-sucle-attribute (attribute)
  (let ((attribute (lem:ensure-attribute attribute t)))
    (make-instance
     'lem-sucle::sucle-attribute
     :underline-p (lem:attribute-underline-p attribute)
     :bold-p (lem:attribute-bold-p attribute)
     :reverse-p (lem:attribute-reverse-p attribute)
     :background (lem:attribute-background attribute)
     :foreground (lem:attribute-foreground attribute)
     )))

;;TODO:subclass lem:attribute in order to attach extra data to attributes
;;have one attribute per overlay?
