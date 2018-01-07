(defpackage #:sound-stuff
  (:use #:cl #:funland
	)
  (:import-from
   #:cl-ffmpeg
 ;  #:dubs
 ;  #:size
   #:rate
   #:bytes-per-sample
   #:channels
   #:audio-format
   #:playsize
 ;  #:dubs2
   ))
(in-package #:sound-stuff)

(defparameter *data* nil)
(defparameter *playback* :stereo16)
(defun alut-test (music &optional (sound-data (make-instance 'cl-ffmpeg::some-sound)))
  #+nil
  (reset sound-data)
  (cl-ffmpeg::get-sound-buff music sound-data)
  (print "data dumped: alut-test ")
  #+nil
  (with-slots (dubs size rate bytes-per-sample channels audio-format
		    dubs2 playsize) sound-data
    (setf (values dubs2 playsize)
	  (convert
	   (case (length dubs)
	     (1 (aref dubs 0))
	     (otherwise (aref dubs 1)))
	   (aref dubs 0)
	   size
	   audio-format
	   *playback*)))
  sound-data)

#+nil
(defun reset (sound-data)
  (with-slots (;;;;dubs2
	       dubs) sound-data
    #+nil
    (when (cffi::pointerp dubs2)    
      (cffi::foreign-free dubs2)
      (setf dubs2 nil))
    #+nil
    (when (typep dubs 'sequence)
      (map nil
	   (lambda (x)
	     (when (cffi::pointerp x)    
	       (cffi::foreign-free x)))
	   dubs))
    (setf dubs nil)))

(defun load-file (file)
  (alut-hello-world)
  (setf *data* (alut-test file))
  #+nil
  (with-slots (dubs2 playsize rate) *data*
    (playmem dubs2 playsize rate)))
(defun play ()
  (al:source-play *source*))
(defun pause ()
  (al:source-pause *source*))

(defun get-buffer ()
 ; (print "get-buffer")
  (or (pop *free-buffers*)
      (al:gen-buffer)))

(defun source-unqueue-buffer (sid)
  (cffi:with-foreign-object (buffer-array :uint)
    (setf (cffi:mem-ref buffer-array :uint) 0)
    (%al:source-unqueue-buffers sid 1 buffer-array)
    (unless (zerop (cffi:mem-ref buffer-array :uint))
      (cffi:mem-aref buffer-array :uint 0))))
(defun free-buffers ()
  (let ((bufs (al:get-source *source* :buffers-processed)))
    (unless (zerop bufs)
      (loop
	 repeat bufs do
	   (let ((buf (source-unqueue-buffer *source*)))
	     (when buf
	       (push buf *free-buffers*)))))
    bufs))

(defun source-queue-buffer (sid buffer)
  (let ((empty? (al:get-source sid :buffers-queued)))
    (cffi:with-foreign-object (buffer-array :uint 1)
      (setf (cffi:mem-aref buffer-array :uint 0)
	    buffer)
      (%al:source-queue-buffers sid 1 buffer-array))
    (when (zerop empty?)
      (unless (eq :paused (al:get-source sid :source-state))
	(al:source-play sid)))))

(defparameter *free-buffers* nil)
(defun playmem (pcm playsize rate)
  (free-buffers)
  (let ((buffer (get-buffer)))
    (al:buffer-data buffer *playback* pcm playsize rate
		    )
    (source-queue-buffer *source* buffer)))

(defun alut-hello-world ()
  (alc:make-context-current *alc-context*)
  (let ((source *source*))
    (al:source source :position (vector 0.0 0.0 0.0))
    (al:source source :velocity (vector 0.0 0.0 0.0))
    (al:source source :gain 1.0)
    (al:source source :pitch 1.0)
    (al:listener :gain 1.0)
    (al:listener :position (vector 0.0 0.0 0.0))
    (al:listener :orientation (vector 0.0 1.0 0.0 0.0 1.0 0.0))))

(defparameter *source* nil)

(defparameter *alc-device* nil)
(defun open-device ()
  (close-device)
  (setf *alc-device* (alc:open-device)))
(defun close-device ()
  (when (cffi:pointerp *alc-device*)
    (alc:close-device *alc-device*))
  (setf *alc-device* nil))
(defparameter *alc-context* nil)
(defun open-context ()
  (close-context)
  (let ((context (alc:create-context *alc-device*)))
    (setf *alc-context* context)
    (alc:make-context-current context)))
(defun close-context ()
  (when (cffi:pointerp *alc-context*)
    (when (cffi:pointer-eq *alc-context* (alc:get-current-context))
      (alc:make-context-current (cffi:null-pointer)))
    (alc:destroy-context *alc-context*))
  (setf *alc-context* nil))

(defun start-al ()
  (open-device)
  (open-context)
  (setf *source* (al:gen-source)))

(defun destroy-al ()
  (close-context)
  (close-device))


(defparameter *al-on?* nil)
(unless *al-on?*
  (start-al)
  (setf *al-on?* t))

(progno
     ;;  (alc:make-context-current fuck::*alc-context*)
     
     (al:listener :position vec)
     (let ((curr *velocity*))
       (setf (aref curr 0) *xvel*)
       (setf (aref curr 1) *yvel*)
       (setf (aref curr 2) *zvel*)
       (al:listener :velocity curr))
     (let ((curr *orientation*)
	   (other (camera-vec-forward *camera*))
	   (other2 (camera-vec-up *camera*)))
       (setf (aref curr 0) (- (aref other 0)))
       (setf (aref curr 1) (- (aref other 1)))
       (setf (aref curr 2) (- (aref other 2)))
       (setf (aref curr 3) (aref other2 0))
       (setf (aref curr 4) (aref other2 1))
       (setf (aref curr 5) (aref other2 2))
       (al:listener :orientation curr)))

#+nil
(when (window:key-j-p :u)
  (al:source *source* :position (list sandbox::*xpos*
				      sandbox::*ypos*
				      sandbox::*zpos*))
  (alut-hello-world))

(defun convert (left right len format playblack-format)
  (let* ((arrcount (case playblack-format
		     ((:stereo8 :stereo16) (* len 2))
		     ((:mono8 :mono16) len)))
	 (arr (case playblack-format
		((:mono8 :stereo8) (cffi:foreign-alloc :uint8 :count arrcount))
		((:mono16 :stereo16) (cffi:foreign-alloc :int16 :count arrcount)))))
    (case playblack-format
      (:mono8
       (values (convert8 left len format arr)
	       len))
      (:mono16
       (values (convert16 left len format arr)
	       (* 2 len)))
      (:stereo8
       (values (interleave8 left right len format (* 2 len) arr)
	       (* len 2)))
      (:stereo16
       (values (interleave16 left right len format (* 2 len) arr)
	       (* len 4))))))

(defmacro clamp (min max x)
  `(max ,min (min ,max ,x)))


;;;;crackling noise when floats above 1.0 or below -1.0?
;;;; clamp or scale floats outside of [-1.0 1.0]?
(eval-when (:compile-toplevel)
  (defparameter *int16-dispatch*
    '(case format
      ((:fltp :flt)
       (let* ((scale 1.0
		#+nil
		(find-max :float
			  1.0
			  -1.0))
	      (scaling-factor (/ 32767.5 scale)))
	 (declare (type single-float scale scaling-factor))
					;	 (print scale)
	 (audio-type :float
		     (round
		      (- (* (clamp -1.0 1.0 value)
			    scaling-factor)
			 0.5)))))
      ((:dblp :dbl)
       (let* ((scale 1.0d0
		#+nil(find-max :double
			       1.0d0
			       -1.0d0))
	      (scaling-factor (/ 32767.5d0 scale)))
	 (declare (type double-float scale scaling-factor))
	 (audio-type :double
		     (round
		      (- 
			 (* 
			  (clamp -1.0d0 1.0d0 value)
			  scaling-factor)
			 0.5d0)))))
      ((:s16 :s16p)
       (audio-type :int16 value))
      ((:s32 :s32p)
       (audio-type :int32 (ash value -16)))
      ((:u8 :u8p)
       (audio-type :uint8 (ash (- value 128) 8)))
      ((:s64 :s64p)
       (audio-type :int64 (ash (the (signed-byte 64) value) -48)))
      (:nb (error "wtf is nb?")))))

;;;length -> samples per channel
(defun interleave16 (left right length format numcount arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum length numcount))
  (macrolet ((audio-type (type form)
	       `(dotimes (index numcount)
		  (setf (cffi:mem-aref arr :int16 index)
			(let* ((little-index (ash index -1))
			       (value (if (oddp index)
					  (cffi:mem-aref left ,type little-index)		    
					  (cffi:mem-aref right ,type little-index))))
			  ,form))))
	     #+nil
	     (find-max (type min max)
	       `(let ((min ,min)
		      (max ,max))
		  (dotimes (index length)
		    (let ((a (cffi:mem-aref left ,type index))		    
			  (b (cffi:mem-aref right ,type index)))
		      (cond ((< a min)
			     (setf min a))
			    ((> a max)
			     (setf max a)))
		      (cond ((< b min)
			     (setf min b))
			    ((> b max)
			     (setf max b)))))
		  (max (abs min)
		       (abs max)))))
    (etouq *int16-dispatch*))
  arr)

(defun convert16 (buffer newlen format arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum newlen))
  (macrolet ((audio-type (type form)
	       `(dotimes (index newlen)
		  (setf (cffi:mem-aref arr :int16 index)
			(let ((value (cffi:mem-aref buffer ,type index)))
			  ,form))))
	     #+nil
	     (find-max (type min max)
	       `(let ((min ,min)
		      (max ,max))
		  (dotimes (index newlen)
		    (let ((a (cffi:mem-aref buffer ,type index)))
		      (cond ((< a min)
			     (setf min a))
			    ((> a max)
			     (setf max a)))))
		  (max (abs min)
		       (abs max)))))
    (etouq *int16-dispatch*))
  arr)

(eval-when (:compile-toplevel)
  (defparameter *uint8-dispatch*
    '(case format
      ((:fltp :flt)
       (let* ((scale 1.2 #+nil
	       (find-max :float
			 1.0
			 -1.0))
	      (scaling-factor (/ 127.5 scale)))
	 (declare (type single-float scale scaling-factor))
	 (audio-type :float
		     (round
		      (- 
			 (* 
			  (clamp -1.0 1.0 value)
			  scaling-factor)
			 0.5)))))
      ((:dblp :dbl)
       (let* ((scale 1.2d0 #+nil
	       (find-max :double
			 1.0d0
			 -1.0d0))
	      (scaling-factor (/ 127.5d0 scale)))
	 (declare (type double-float scale scaling-factor))
	 (audio-type :double
		     (round
		      (- 
			 (* 
			  (clamp -1.0d0 1.0d0 value)
			  scaling-factor)
			 0.5d0)))))
      ((:s16 :s16p)
       (audio-type :int16 (+ 128 (ash value -8))))
      ((:s32 :s32p)
       (audio-type :int32 (+ 128 (ash value -24))))
      ((:u8 :u8p)
       (audio-type :uint8 value))
      ((:s64 :s64p)
       (audio-type :int64 (+ 128 (ash (the (signed-byte 64) value) -56))))
      (:nb (error "wtf is nb?")))))

(defun interleave8 (left right length format numcount arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum length numcount))
  (macrolet ((audio-type (type form)
	       `(dotimes (index numcount)
		  (setf (cffi:mem-aref arr :uint8 index)
			(let* ((little-index (ash index -1))
			       (value (if (oddp index)
					  (cffi:mem-aref left ,type little-index)		    
					  (cffi:mem-aref right ,type little-index))))
			  ,form))))
	     #+nil
	     (find-max (type min max)
	       `(let ((min ,min)
		      (max ,max))
		  (dotimes (index length)
		    (let ((a (cffi:mem-aref left ,type index))		    
			  (b (cffi:mem-aref right ,type index)))
		      (cond ((< a min)
			     (setf min a))
			    ((> a max)
			     (setf max a)))
		      (cond ((< b min)
			     (setf min b))
			    ((> b max)
			     (setf max b)))))
		  (max (abs min)
		       (abs max)))))
    (etouq *uint8-dispatch*))
  arr)

(defun convert8 (buffer newlen format arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum newlen))
  (macrolet ((audio-type (type form)
	       `(dotimes (index newlen)
		  (setf (cffi:mem-aref arr :uint8 index)
			(let ((value (cffi:mem-aref buffer ,type index)))
			  ,form))))
	     #+nil
	     (find-max (type min max)
	       `(let ((min ,min)
		      (max ,max))
		  (dotimes (index newlen)
		    (let ((a (cffi:mem-aref buffer ,type index)))
		      (cond ((< a min)
			     (setf min a))
			    ((> a max)
			     (setf max a)))))
		  (max (abs min)
		       (abs max)))))
    (etouq *uint8-dispatch*))
  arr)
