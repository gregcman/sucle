(defpackage #:pathwise
  (:use #:cl)
  (:export

   #:file-string
   #:byte-read
   #:expand-paths))


(in-package :pathwise)

;;;load a file into a string
(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

;;;load a file into a byte-array
(defun byte-read (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let* ((len (file-length stream))
	   (data (make-array len :element-type '(unsigned-byte 8))))
      (dotimes (n len)
	(setf (aref data n) (read-byte stream)))
      data)))

;;;takes a tree list structure where the car of each list is the folder name ex: #P"root/"
;;;and the other items in the rest of the list are either more lists or just plain strings.
;;;returns a flat list which is the collection of all the resulting pathnames
(defun expand-paths (dir-list)
  (let (acc) 
      (labels ((rec (x currentpath)
		 (if (consp x)
		     (dolist (sub (cdr x))
		       (rec sub (merge-pathnames (car x) currentpath)))
		     (push (merge-pathnames x currentpath) acc))))
	(rec dir-list #P"") acc)))

