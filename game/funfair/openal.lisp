(defpackage #:sound-stuff
  (:use #:cl #:funland
	)
  (:import-from
   #:cl-ffmpeg
   #:rate
   #:bytes-per-sample
   #:channels
   #:audio-format
   ))
(in-package #:sound-stuff)


(defmacro subprocess ((&rest vars) &body body)
  (let ((syms (mapcar (lambda (x) (gensym (string x))) vars)))
    `(bordeaux-threads:make-thread
      (let ,(mapcar #'list syms vars)
	(lambda ()
	  (let ,(mapcar #'list vars syms)
	    ,@body))))))
(defmacro iosub (&body body)
  `(subprocess (*standard-output* *standard-input* *terminal-io*)
     ,@body))


(defmacro floatify (x)
  `(coerce ,x 'single-float))
(defmacro clamp (min max x)
  `(max ,min (min ,max ,x)))

#+nil
(progno     
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
((alut-hello-world)
 (defun alut-hello-world (&optional (source *source*))
   (al:source source :position (vector 0.0 0.0 0.0))
   (al:source source :velocity (vector 0.0 0.0 0.0))
   (al:source source :gain 1.0)
   (al:source source :pitch 1.0)))

(defclass datobj ()
  ((data :initform nil)
   (source :initform nil)
   (playback :initform (or; :mono8
			     ;:mono16
			     ;:stereo8
			     :stereo16
			     ))
   (time-remaining :initform 0.0)
   (used-buffers :initform (make-hash-table :test 'eql))
   (cancel :initform t)))

(defparameter *data* nil)
;;do not switch source formats!!!!
(defparameter *datobj* (make-instance 'datobj))
(defun load-file (file &optional (datobj *datobj*))
  (with-slots (source) datobj
    (when source (al:delete-source source))
    (setf source (al:gen-source)))
  (setf *data* (alut-test file datobj)))
(defun play (&optional (datobj *datobj*))
  (with-slots (source) datobj
    (al:source-play source)))
(defun pause (&optional (datobj *datobj*))
  (with-slots (source) datobj
    (al:source-pause source)))
(defun stop (&optional (datobj *datobj*))
  (with-slots (cancel source used-buffers time-remaining) datobj
    (setf cancel t)
    (al:source-stop source)
    (free-buffers datobj)
    (free-buffers-hash used-buffers)
    (setf time-remaining 0.0)))


(defun getpacket (dataobj)
  (let ((format (slot-value dataobj 'playback)))
    (lambda (data samples channels audio-format rate)
      (multiple-value-bind (array playsize)
	  (sound-stuff::convert
	   (case channels
	     (1 (cffi:mem-aref data :pointer 0))
	     (otherwise (cffi:mem-aref data :pointer 1)))
	   (cffi:mem-aref data :pointer 0)
	   samples
	   audio-format
	   format)
	(sound-stuff::playmem format array playsize rate dataobj)
	(cffi::foreign-free array))
      nil)))
(defun alut-test (music datobj)
  (let ((music (cl-ffmpeg::init-music-stuff music))
	(packetfun (getpacket datobj)))
    (with-slots (cancel time-remaining) datobj
      (setf cancel nil
	    time-remaining 0.0)
      (tagbody rep
	 (when (cl-ffmpeg::get-sound-buff music packetfun)
	   (loop
	      (progn
		(when cancel (return))
		(free-buffers datobj)
		(let ((almost 0.5))
		  (if (<= time-remaining almost)
		      (go rep)
		      (sleep (max 0.0 (- time-remaining almost)))
		      )))))))
    (cl-ffmpeg::free-music-stuff music)
 ;   (print "data dumped: alut-test ")
    music))


(defun reset-listener ()
  (al:listener :gain 1.0)
  (al:listener :position (vector 0.0 0.0 0.0))
  (al:listener :orientation (vector 0.0 1.0 0.0 0.0 1.0 0.0)))

(defun source-unqueue-buffer (datobj)
  (with-slots ((sid source) used-buffers) datobj
      (let ((value (%source-unqueue-buffer sid)))
	(when value
	  (remhash value used-buffers))
	value)))
(defun playmem (format pcm playsize rate datobj)
  (with-slots (source) datobj
    (let ((buffer (get-buffer)))
      (al:buffer-data buffer format pcm playsize rate)
      (source-queue-buffer datobj buffer))))
(defun source-queue-buffer (datobj buffer)
  (with-slots (time-remaining (sid source) used-buffers) datobj
    (incf time-remaining (buffer-seconds buffer))
    (setf (gethash buffer used-buffers) t)
    (%source-queue-buffer sid buffer)))
(defun free-buffers (datobj)
  (with-slots (source time-remaining) datobj
    (multiple-value-bind (time bufs)
	(%free-buffers source datobj)
      (decf time-remaining time)
      (values time bufs))))

(defun free-buffers-hash (hash)
  (bordeaux-threads:with-lock-held (*free-buffers-lock*)
    (funland::dohash (k v) hash
      (declare (ignore v))
      (push k *free-buffers*)))
  (clrhash hash))

(defun get-buffer ()
  (or (bordeaux-threads:with-lock-held (*free-buffers-lock*)
	(pop *free-buffers*))
      (al:gen-buffer)))
(defparameter *free-buffers* nil)
(defparameter *free-buffers-lock* (bordeaux-threads:make-lock "free albuffers"))
(defun %free-buffers (sid datobj)
  (let ((bufs (al:get-source sid :buffers-processed))
	(time 0.0))
    (when (< 0 bufs)
      (dotimes (i bufs)
	(let ((buf (source-unqueue-buffer datobj)))
	  (when buf
	    (incf time (buffer-seconds buf))
	    (bordeaux-threads:with-lock-held (*free-buffers-lock*)
	      (push buf *free-buffers*))))))
    (values
     time
     bufs)))

(defun %source-unqueue-buffer (sid)
  (cffi:with-foreign-object (buffer-array :uint 1)
    (setf (cffi:mem-aref buffer-array :uint 0) 0)
    (%al:source-unqueue-buffers sid 1 buffer-array)
    (let ((item (cffi:mem-aref buffer-array :uint 0)))
      (if (zerop item)
	  nil
	  item))))

(defun %source-queue-buffer (sid buffer)
  (let ((empty? (al:get-source sid :buffers-queued)))
    (cffi:with-foreign-object (buffer-array :uint 1)
      (setf (cffi:mem-aref buffer-array :uint 0)
	    buffer)
      (%al:source-queue-buffers sid 1 buffer-array))
    (when (zerop empty?)
      (unless (eq :paused (al:get-source sid :source-state))
	(al:source-play sid)))))

;;;;;
(defun buffer-seconds (buffer)
  (let ((size (floatify (al:get-buffer buffer :size)))
	(bits (floatify (al:get-buffer buffer :bits)))
	(channels (floatify (al:get-buffer buffer :channels)))
	(frequency (floatify (al:get-buffer buffer :frequency))))
    ;;   (print (list size bits channels frequency))
    (let ((denom (* channels bits frequency)))
      (/ (* size 8) denom))
       ))

;;;;initialization
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
  (alc:make-context-current *alc-context*)
  (reset-listener))
(defun destroy-al ()
  (close-context)
  (close-device)
  (setf *free-buffers* nil)
  (setf *al-on?* nil))
(defparameter *al-on?* nil)
(defun really-start ()
  (unless *al-on?*
    (start-al)
    (setf *al-on?* t)))
(eval-when (:execute :load-toplevel)
  (really-start))



;;;;ffmpeg format to openal format
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
       (values (interleave8 left right format (* 2 len) arr)
	       (* len 2)))
      (:stereo16
       (values (interleave16 left right format (* 2 len) arr)
	       (* len 4))))))

;;	DC DAC Modeled -> [-1.0 1.0] -> [-32768 32767]
;;      apple core audo, alsa, matlab, sndlib -> (lambda (x) (* x #x8000))
;;
;;;;crackling noise when floats above 1.0 or below -1.0?
;;;; clamp or scale floats outside of [-1.0 1.0]?
(eval-when (:compile-toplevel)
  (defparameter *int16-dispatch*
    '(case format
      ((:fltp :flt)
       (let* ((scale 1.0)
	      (scaling-factor (/ 32768.0 scale)))
	 (declare (type single-float scale scaling-factor))
	 (audio-type :float
		     (clamp
		      #x-8000 #x7fff
		      (the fixnum
			   (round
			    (* value
			       scaling-factor)))))))
      
      ((:dblp :dbl)
       (let* ((scale 1.0d0)
	      (scaling-factor (/ 32768.0d0 scale)))
	 (declare (type double-float scale scaling-factor))
	 (audio-type :double
		     (clamp
		      #x-8000 #x7fff
		      (the fixnum
			   (round
			    (* 
			     value
			     scaling-factor)))))))
      ((:s16 :s16p)
       (audio-type :int16 value))
      ((:s32 :s32p)
       (audio-type :int32 (ash value -16)))
      ((:u8 :u8p)
       (audio-type :uint8 (ash (- value 128) 8)))
      ((:s64 :s64p)
       (audio-type :int64 (ash (the (signed-byte 64) value) -48)))
      (:nb (error "wtf is nb?")))))

(deftype carray-index ()
  `(integer 0 ,(load-time-value (ash most-positive-fixnum -3))))
;;;length -> samples per channel
(defun interleave16 (left right format numcount arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type carray-index numcount))
  (macrolet ((audio-type (type form)
	       `(dotimes (index numcount)
		  (setf (cffi:mem-aref arr :int16 index)
			(let* ((little-index (ash index -1))
			       (value (if (oddp index)
					  (cffi:mem-aref left ,type little-index)		    
					  (cffi:mem-aref right ,type little-index))))
			  ,form)))))
    (etouq *int16-dispatch*))
  arr)

(defun convert16 (buffer newlen format arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type carray-index newlen))
  (macrolet ((audio-type (type form)
	       `(dotimes (index newlen)
		  (setf (cffi:mem-aref arr :int16 index)
			(let ((value (cffi:mem-aref buffer ,type index)))
			  ,form)))))
    (etouq *int16-dispatch*))
  arr)

(eval-when (:compile-toplevel)
  (defparameter *uint8-dispatch*
    '(case format
      ((:fltp :flt)
       (let* ((scale 1.0)
	      (scaling-factor (/ 128.0 scale)))
	 (declare (type single-float scale scaling-factor))
	 (audio-type :float
		     (clamp
		      -128 127
		      (the fixnum
			   (round
			    (* value
			       scaling-factor)))))))
      ((:dblp :dbl)
       (let* ((scale 1.0d0)
	      (scaling-factor (/ 128d0 scale)))
	 (declare (type double-float scale scaling-factor))
	 (audio-type :double
		     (clamp
		      -128 127
		      (the fixnum
			   (round
			    (*
			     value
			     scaling-factor)))))))
      ((:s16 :s16p)
       (audio-type :int16 (+ 128 (ash value -8))))
      ((:s32 :s32p)
       (audio-type :int32 (+ 128 (ash value -24))))
      ((:u8 :u8p)
       (audio-type :uint8 value))
      ((:s64 :s64p)
       (audio-type :int64
	(+ 128 (ash (the (unsigned-byte 64) value) -56))))
      (:nb (error "wtf is nb?")))))

(defun interleave8 (left right format numcount arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type carray-index numcount))
  (macrolet ((audio-type (type form)
	       `(dotimes (index numcount)
		  (setf (cffi:mem-aref arr :uint8 index)
			(let* ((little-index (ash index -1))
			       (value (if (oddp index)
					  (cffi:mem-aref left ,type little-index)		    
					  (cffi:mem-aref right ,type little-index))))
			  ,form)))))
    (etouq *uint8-dispatch*))
  arr)

(defun convert8 (buffer newlen format arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type carray-index newlen))
  (macrolet ((audio-type (type form)
	       `(dotimes (index newlen)
		  (setf (cffi:mem-aref arr :uint8 index)
			(let ((value (cffi:mem-aref buffer ,type index)))
			  ,form)))))
    (etouq *uint8-dispatch*))
  arr)


#|

	Int to Float
	Float to Int*
	Transparency
	Used By
0)
	((integer + .5)/(0x7FFF+.5)
	float*(0x7FFF+.5)-.5
	Up to at least 24-bit
	DC DAC Modeled
1)
	(integer / 0x8000)
	float * 0x8000
	Up to at least 24-bit
	Apple (Core Audio)1, ALSA2, MatLab2, sndlib2
2)
	(integer / 0x7FFF)
	float * 0x7FFF
	Up to at least 24-bit
	Pulse Audio2
3)
	(integer / 0x8000)
	float * 0x7FFF
	Non-transparent
	PortAudio1,2, Jack2, libsndfile1,3
4)
	(integer>0?integer/0x7FFF:integer/0x8000)
	float>0?float*0x7FFF:float*0x8000
	Up to at least 24-bit
	At least one high end DSP and A/D/A manufacturer.2,4 XO Wave 1.0.3.
5)
	Uknown
	float*(0x7FFF+.49999)
	Unknown
	ASIO2
*obviously, rounding or dithering may be required here.
Note that in the case of IO APIs, drivers are often responsible for conversions. The conversions listed here are provided by the API.

from:: http://blog.bjornroche.com/2009/12/int-float-int-its-jungle-out-there.html
|#
