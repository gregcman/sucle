(in-package #:sandbox)

(defun load-png (name filename)
  (setf (gethash name picture-library)
	(flip-image (opticl:read-png-file filename))))

(defun load-shader-file (name)
  (file-string
   (merge-pathnames name (merge-pathnames shaderdir resdir))))

(defun load-shit (tex-data name width height)
  (setf (gethash name texture-library)
	(create-texture-wot tex-data width height)))

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun chunk-byte-offset (x z)
  (* 4
     (+
      (mod x 32)
      (* 32 (mod z 32)))))

(defun byte-read (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let* ((len (file-length stream))
	   (data (make-array len :element-type '(unsigned-byte 8))))
      (dotimes (n len)
	(setf (aref data n) (read-byte stream)))
      data)))

(defparameter testchunk (sandbox::byte-read "/home/imac/Downloads/cNBT-master/testdata/hell.mcr"))
(defun tonum (nums)
  (let ((danum 1))
    (setf nums (reverse nums))
    (dotimes (n (length nums))
      (incf danum (* (expt 256 n) (elt nums n))))
    danum))

(defun my-subseq (array start amount)
  (subseq array start (+ start amount)))

(defun mcr-chunk (mcrdata x z)
  (let* ((initial (chunk-byte-offset x z))
	 (firstdata (subseq mcrdata initial (+ initial 4)))
	 (region-offset (tonum (subseq firstdata 0 3)))
	 (page-length (elt firstdata 3))
	 (time-stamp (tonum (my-subseq mcrdata (+ 4096 initial) 4)))
	 (chunkdata (my-subseq mcrdata (* 4096 region-offset) (* page-length 4096)))
	 (chunklength (tonum (my-subseq chunkdata 0 4)))
	 (compression-type (elt chunkdata 4))
	 (compressed-chunk-data (my-subseq chunkdata 5 (1- chunklength))))
    (print (list region-offset page-length time-stamp))
    (if (= 2 compression-type)
	(chipz:decompress nil 'chipz:zlib compressed-chunk-data)
	(chipz:decompress nil 'chipz:gzip compressed-chunk-data))))

(defun nbt-compound ())
(defun nbt-list ())

(defun flatten (obj)
  (do* ((result (list obj))
        (node result))
       ((null node) (delete nil result))
    (cond ((consp (car node))
           (when (cdar node) (push (cdar node) (cdr node)))
           (setf (car node) (caar node)))
          (t (setf node (cdr node))))))

(defun fatten (some-array)
  (let* ((total-size (array-total-size some-array))
	 (new-array (make-array total-size)))
    (dotimes (x total-size)
      (setf (aref new-array x) (row-major-aref some-array x)))
    new-array))

(defun flip-image(darray)
  (let* ((dims (array-dimensions darray))
	 (myray (make-array dims)))   
    (dotimes (w (first dims))
      (dotimes (h (second dims))
	(dotimes (val (third dims))
	  (setf
	   (aref myray (- (first dims) w 1) h val)
	   (aref darray w h val)))))   
    myray))

(defun get-file-paths-and-metadata (x currentpath)
  (if (consp x)
      (let* ((dirnombre (first x))
	     (babies (cdr x)))
	(mapcar
	 (lambda (n)
	   (get-file-paths-and-metadata
	    n
	    (merge-pathnames dirnombre currentpath)))
	 babies))
      (progn
	(list x (merge-pathnames x currentpath)))))



