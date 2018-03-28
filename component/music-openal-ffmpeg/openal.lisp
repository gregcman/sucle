(defpackage #:music
  (:use #:cl #:utility)
  (:import-from
   #:cl-ffmpeg
   #:rate
   #:bytes-per-sample
   #:channels
   #:audio-format)
  (:export
   #:play-at))
(in-package #:music)


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

(defparameter *format* (or :mono8
			   :mono16
			   :stereo8
			   :stereo16
			   ))
(defclass datobj ()
  ((source :initform nil)
   (playback :initform *format*)
   (time-remaining :initform 0)
   (used-buffers :initform (make-hash-table :test 'eql))
   ;;set to t or 'aborted to stop streaming from disk to al
   ;;cleaned up when source stops with t, exits immediately with 'aborted
   (status :initform nil) 
   (data :initform nil)))

(defun datobj-freeable? (&optional (datobj *datobj*))
  (eq :stopped
      (al:get-source (slot-value datobj 'source) :source-state)))

(defun free-datobj (&optional (datobj *datobj*))
  (with-slots (source used-buffers data time-remaining) datobj
    (al:source-stop source)
    (al:source source :buffer 0)
    (free-buffers-hash used-buffers)
    (setf time-remaining 0)
    (cl-ffmpeg::free-music-stuff data)   
    (al:delete-source source)))

(defun delete-one-buffer (buffer)
  (cffi:with-foreign-object (buffer-array :uint 1)
    (setf
     (cffi:mem-aref buffer-array :uint 0)
     buffer)
    (%al:delete-buffers 1 buffer-array)))

(defun free-a-buffer (datobj)
  ;;;(bordeaux-threads:with-lock-held (*free-buffers-lock*))
  (let ((buf (al:get-source (slot-value datobj 'source) :buffer)))
    (if (or (= buf 0)
	    (= buf 1))
	(format t "wut ~a" buf)
	(delete-one-buffer buf)
					;	  (push buf *free-buffers*)
	)))

(defparameter *task* (lparallel:make-channel))
(defun play-at (sound x y z pitch volume)
  (when (> 128 (total-handles))
    (typecase sound
      ((or pathname string)
       (when (pathnamep sound)
	 (setf sound (namestring sound)))
       (lparallel:submit-task
	*task*
	(let ((format *format*))
	  (lambda (filename x y z)
	    (let ((*format* format))
	      (multiple-value-bind (datobj source) (load-file filename)
		(when datobj
		  (%al:source-3f source :position
				 (floatify x)
				 (floatify y)
				 (floatify z))
		  (al:source source :velocity (load-time-value (vector 0.0 0.0 0.0)))
		  (al:source source :gain volume)
		  (al:source source :pitch pitch)
		  (push-sound datobj)
		  (values datobj source))))))
	sound x y z))
      (preloaded-music (play-preloaded-at sound x y z pitch volume)))))

(defparameter *datobj* nil)
;;do not switch source formats!!!!
(defun load-file (music-file)
  (let ((ffmpeg-stuff (cl-ffmpeg::init-music-stuff music-file)))
    (when ffmpeg-stuff
      (let ((datobj (make-instance 'datobj)))
	(setf *datobj* datobj)
	(with-slots (source data) datobj
	  (setf source (al:gen-source))	
	  (setf data ffmpeg-stuff)
	  (values datobj
		  source))))))
(defun play (&optional (datobj *datobj*))
  (with-slots (source) datobj
    (al:source-play source)))
(defun pause (&optional (datobj *datobj*))
  (with-slots (source) datobj
    (al:source-pause source)))
(defun stop (&optional (datobj *datobj*))
  (with-slots (status) datobj
    (setf status 'aborted)))

(defun push-sound (datobj)
  (lparallel.queue:push-queue datobj *new-sounds*)
  (when (or (not *sound-thread*)
	    (not (bordeaux-threads:thread-alive-p *sound-thread*)))
    (start-poller))
  datobj)

(defun total-handles ()
  (+ (lparallel.queue:queue-count *new-sounds*)
     (hash-table-count *datobjs*)))

(defparameter *new-sounds* (lparallel.queue:make-queue))
(defparameter *sound-thread* nil)
(defparameter *stop* nil)
(defparameter *datobjs* (make-hash-table :test 'eql))
(defparameter *datobjs-lock* (bordeaux-threads:make-lock "datobjs"))
(defun poller ()
  (unwind-protect
       (tagbody repeat
	  (dotimes (i (lparallel.queue:queue-count *new-sounds*))
	    (bordeaux-threads:with-lock-held (*datobjs-lock*)
	      (multiple-value-bind (value exists?)
		  (lparallel.queue:try-pop-queue *new-sounds*)
		(when exists?
		  (setf (gethash value *datobjs*) t)))))
	  (unless *stop*
	    (let ((flag nil))
	      (bordeaux-threads:with-lock-held (*datobjs-lock*)
		(dohash (obj dummy) *datobjs*
		  (declare (ignorable dummy))
		  (multiple-value-bind (destructible?) (update-obj obj) ;;finished or cancelled
		    (if destructible?
			(progn (destroy-obj obj)
			       (remhash obj *datobjs*))
			(setf flag t))))) ;;still playing
	      (when flag
		(sleep 0.1)
		(go repeat)))))
    (cleanup-poller)))

(defun update-obj (obj)
  (etypecase obj
    (datobj
     (or (eq 'aborted (slot-value obj 'status))
	 (progn
	   (update-playable obj)
	   (let ((value (slot-value obj 'status)))
	     (or (eq value 'aborted)
		 (and (eq value t)
		      (datobj-freeable? obj)))))))
    (integer (eq :stopped
		 (al:get-source obj :source-state)))))

(defun destroy-obj (obj)
  (etypecase obj
    (datobj
     (free-datobj obj))
    (integer
     (al:source-stop obj)
     (al:delete-source obj)
     )))

(defun cleanup-poller ()
  (bordeaux-threads:with-lock-held (*datobjs-lock*)
    (dohash (k v) *datobjs*
      (declare (ignore v))
      (destroy-obj k))
    (clrhash *datobjs*)
    (setf *datobj* nil)

    (let ((thread *sound-thread*))
      (when thread
	(unless (eq thread (bordeaux-threads:current-thread))
	  (bordeaux-threads:destroy-thread thread))))
    (setf *sound-thread* nil)))

(defun start-poller ()
  (setf *sound-thread* (iosub (poller))))

(defun update-playable (datobj)
  (macrolet ((exit (x)
	       `(return-from exit ,x)))
    (block exit
      (with-slots (status time-remaining (format playback) (music data)) datobj
	(let* ((rate (slot-value 
		      (slot-value 
		       music
		       'cl-ffmpeg::sound)
		      'cl-ffmpeg::rate))
	       (threshold 10.0)
	       (target 10.0))
	  (when (not rate)
	    (setf status 'aborted)
	    (exit 'aborted))
	  (when (eq status 'aborted)
	    (exit 'aborted))
	  (tagbody move
	     (free-buffers datobj)
	     (when (>= (* rate threshold) time-remaining)
	       (let ((target-samples (* rate target)))
		 (setf
		  status 
		  (cl-ffmpeg::%get-sound-buff (data samples channels audio-format rate) music
		    (flet ((conv (arr)
			     (multiple-value-bind (pcm playsize)
				 (convert
				  channels
				  data
				  samples
				  audio-format
				  format
				  arr)
			       (let ((buffer (get-buffer)))
				 (al:buffer-data buffer format pcm playsize rate)
				 (source-queue-buffer datobj buffer)))))	      
		      (let ((arrcount (ecase format
					((:stereo8 :stereo16) (* samples 2))
					((:mono8 :mono16) samples))))
			(ecase format
			  ((:mono8 :stereo8)
			   (cffi:with-foreign-object (arr :uint8 arrcount)
			     (conv arr)))
			  ((:mono16 :stereo16)
			   (cffi:with-foreign-object (arr :int16 arrcount)
			     (conv arr))))))
		    (when (eq status 'aborted)
		      (return 'aborted))
		    (decf target-samples samples)
		    (when (>= 0 target-samples)
		      (return nil)))))
	       (when
		   (eq status nil)
		 (go move)))))))))

(defclass preloaded-music ()
  ((buffers :initform nil)
   (complete :initform nil)
   (info :initform nil)))

(defun load-all (file &optional (format *format*))
  (when (pathnamep file)
    (setf file (namestring file)))
  (let ((music nil))
    (unwind-protect
	 (progn
	   (setf music (cl-ffmpeg::init-music-stuff file))
	   (let ((rate (slot-value 
			(slot-value 
			 music
			 'cl-ffmpeg::sound)
			'cl-ffmpeg::rate))
		 (sound-buffers ())
		 (completed? nil))
	     (when (not rate)
	       (return-from load-all (values nil nil)))
	     (when
		 (cl-ffmpeg::%get-sound-buff (data samples channels audio-format rate) music
		   (flet ((conv (arr)
			    (multiple-value-bind (pcm playsize)
				(convert
				 channels
				 data
				 samples
				 audio-format
				 format
				 arr)
			      (let ((buffer (get-buffer)))
				(al:buffer-data buffer format pcm playsize rate)
				(push buffer sound-buffers)))))
		     (let ((arrcount (ecase format
				       ((:stereo8 :stereo16) (* samples 2))
				       ((:mono8 :mono16) samples))))
		       (ecase format
			 ((:mono8 :stereo8)
			  (cffi:with-foreign-object (arr :uint8 arrcount)
			    (conv arr)))
			 ((:mono16 :stereo16)
			  (cffi:with-foreign-object (arr :int16 arrcount)
			    (conv arr)))))))
	       (setf completed? t))
	     (let ((inst
		    (make-instance 'preloaded-music)))
	       (with-slots (buffers complete info) inst
		 (setf buffers (coerce (nreverse sound-buffers) 'vector)
		       complete completed?
		       info (slot-value music 'cl-ffmpeg::sound)))
	       inst)))
      (cl-ffmpeg::free-music-stuff music))))

(defun play-preloaded-at (preloaded x y z pitch volume)
  (let ((source (al:gen-source)))
    (%al:source-3f source :position
		   (floatify x)
		   (floatify y)
		   (floatify z))
    (al:source source :velocity (load-time-value (vector 0.0 0.0 0.0)))
    (al:source source :gain volume)
    (al:source source :pitch pitch)
    (al:source-queue-buffers source (slot-value preloaded 'buffers))
    (al:source-play source)
    (push-sound source)))

(defun free-preloaded (preloaded)
  (al:delete-buffers (slot-value preloaded 'buffers)))


(defun reset-listener ()
  (al:listener :gain 1.0)
  (al:listener :position (vector 0.0 0.0 0.0))
  (al:listener :velocity (vector 0.0 0.0 0.0))
  (al:listener :orientation (vector 0.0 1.0 0.0 0.0 1.0 0.0)))

(defun source-queue-buffer (datobj buffer)
  (with-slots (time-remaining (sid source) used-buffers) datobj
    (incf time-remaining (buffer-samples buffer))
    (setf (gethash buffer used-buffers) t)
   
    (%source-queue-buffer sid buffer)
    (unless (eq :paused (al:get-source sid :source-state))
      (al:source-play sid))))
(defun free-buffers (datobj)
  (with-slots (source time-remaining) datobj
    (multiple-value-bind (time bufs)
	(%free-buffers source datobj)
      (decf time-remaining time)
      (values time bufs))))

(defun free-buffers-hash (hash)
  ;(bordeaux-threads:with-lock-held (*free-buffers-lock*))
  (dohash (k v) hash
    (declare (ignore v))
    (delete-one-buffer k)
					;      (push k *free-buffers*)
    )
  (clrhash hash))

#+nil ;;;sources and buffers share namespace?
(defun free-buffer-integrity? ()
  (let ((a (reduce #'max *free-buffers* :initial-value 1))
	(b (1+ (length *free-buffers*))))
    (if
     (= a
	b)
     t
     (format t "max: ~a len: ~a" a b))))
(defun get-buffer ()
  #+nil
  (or (bordeaux-threads:with-lock-held (*free-buffers-lock*)
	(pop *free-buffers*)))
  (al:gen-buffer))
#+nil
(defparameter *free-buffers* nil)
#+nil
(defparameter *free-buffers-lock* (bordeaux-threads:make-lock "free albuffers"))
(defun %free-buffers (sid datobj)
  (let ((bufs (al:get-source sid :buffers-processed))
	(time 0)
	(used-buffers (slot-value datobj 'used-buffers)))
    (when (< 0 bufs)
      (cffi:with-foreign-object (buffer-array :uint bufs)
	(dotimes (index bufs)
	  (setf (cffi:mem-aref buffer-array :uint index) 0))
	(%al:source-unqueue-buffers sid bufs buffer-array)
	(dotimes (index bufs)
	  (let ((buf (cffi:mem-aref buffer-array :uint index)))
	    (when (not (zerop buf))
	      (remhash buf used-buffers)
	      (incf time (buffer-samples buf))
	      ;(bordeaux-threads:with-lock-held (*free-buffers-lock*))
	      (delete-one-buffer buf)
;		(push buf *free-buffers*)
		)))))
    (values
     time
     bufs)))

(defun %source-queue-buffer (sid buffer)
  (cffi:with-foreign-object (buffer-array :uint 1)
    (setf (cffi:mem-aref buffer-array :uint 0)
	  buffer)
    (%al:source-queue-buffers sid 1 buffer-array)))

(defun buffer-samples (buffer)
  (let ((size (floatify (al:get-buffer buffer :size))) ;byte count
	(bits (floatify (al:get-buffer buffer :bits))) ;;bit depth eg: 16 or 8
	(channels (floatify (al:get-buffer buffer :channels))))
    (/ (* size 8)
       (* channels bits))))

;;;;;
#+nil
(defun buffer-seconds (buffer)
  (let ((size (floatify (al:get-buffer buffer :size))) ;byte count
	(bits (floatify (al:get-buffer buffer :bits))) ;;bit depth eg: 16 or 8
	(channels (floatify (al:get-buffer buffer :channels)))
	(frequency (floatify (al:get-buffer buffer :frequency))))
    ;;   (print (list size bits channels frequency))
    (/ (* size 8)
       (* channels bits frequency))
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
    (alc:make-context-current (cffi:null-pointer))
    (alc:destroy-context *alc-context*))
  (setf *alc-context* nil))
(defun start-al ()
  (open-device)
  (open-context)
  (alc:make-context-current *alc-context*)
  (start-poller)
  (reset-listener))
(defun destroy-al ()
  (close-context)
  (close-device)
  (setf *free-buffers* nil)
  (clrhash *datobjs*)
  (cleanup-poller)
  (setf *al-context* nil))
(defparameter *al-context* nil)
(defun really-start ()
  (unless *al-context*
    (start-al)
    (setf *al-context* (cons "OpenAL context" nil))))

(defun restart-al ()
  (destroy-al)
  (really-start))

(defun planar-p (format)
  (case format
    ((:fltp :dblp :s16p :s32p :u8p :s64p) t)))

;;;;ffmpeg format to openal format
(defun convert (channels data len format playback-format arr) 
  (ecase channels
    (1 (let ((channel (cffi:mem-aref data :pointer 0)))
	 (case playback-format
	   (:mono8
	    (values (array->uint8 channel format len arr)
		    len))
	   (:mono16
	    (values (array->int16 channel format len arr)
		    (* 2 len)))
	   (:stereo8
	    (values (mono->stereo8 channel format (* 2 len) arr)
		    (* len 2)))
	   (:stereo16
	    (values (mono->stereo16 channel format (* 2 len) arr)
		    (* len 4))))))
    (2
     (if (planar-p format)
	 (let ((left (cffi:mem-aref data :pointer 0))
	       (right (cffi:mem-aref data :pointer 1)))
	   (case playback-format
	     (:mono8
	      (values (planar-stereo->mono8 left right format len arr)
		      len))
	     (:mono16
	      (values (planar-stereo->mono16 left right format len arr)
		      (* 2 len)))
	     (:stereo8
	      (values (planar-stereo->stereo8 left right format (* 2 len) arr)
		      (* len 2)))
	     (:stereo16
	      (values (planar-stereo->stereo16 left right format (* 2 len) arr)
		      (* len 4)))))
	 (let ((channel (cffi:mem-aref data :pointer 0)))
	   (case playback-format
	     (:mono8
	      (values (interleaved-stereo->mono8 channel format len arr)
		      len))
	     (:mono16
	      (values (interleaved-stereo->mono16 channel format len arr)
		      (* 2 len)))
	     (:stereo8
	      (values (array->uint8 channel format (* 2 len) arr)
		      (* len 2)))
	     (:stereo16
	      (values (array->int16 channel format (* 2 len) arr)
		      (* len 4)))))))))

;;	DC DAC Modeled -> [-1.0 1.0] -> [-32768 32767]
;;      apple core audo, alsa, matlab, sndlib -> (lambda (x) (* x #x8000))
;;
;;;;crackling noise when floats above 1.0 or below -1.0?
;;;; clamp or scale floats outside of [-1.0 1.0]?
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *int16-dispatch*
    '(ecase format
      ((:flt :fltp)
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
      
      ((:dbl :dblp)
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
(defun planar-stereo->stereo16 (left right format numcount arr)
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

(defun interleaved-stereo->mono16 (channel format numcount arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type carray-index numcount))
  (macrolet ((audio-type (type form)
	       `(dotimes (index numcount)
		  (setf (cffi:mem-aref arr :int16 index)
			(let* ((indexa (ash index 1))
			       (indexb (+ 1 indexa))
			       (a (cffi:mem-aref channel ,type indexa))
			       (b (cffi:mem-aref channel ,type indexb))
			       (c (the ,(case type
					      (:double 'double-float)
					      (:float 'single-float)
					      (otherwise 'fixnum))
				       (+ a b)))
			       (value ,(case type
					     (:double '(/ c 2.0d0))
					     (:float '(/ c 2.0))
					     (otherwise '(ash c -1)))))
			  ,form)))))
    (etouq *int16-dispatch*))
  arr)


(defun planar-stereo->mono16 (left right format numcount arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type carray-index numcount))
  (macrolet ((audio-type (type form)
	       `(dotimes (index numcount)
		  (setf (cffi:mem-aref arr :int16 index)
			(let* ((a (cffi:mem-aref left ,type index))
			       (b (cffi:mem-aref right ,type index))
			       (c (the ,(case type
					      (:double 'double-float)
					      (:float 'single-float)
					      (otherwise 'fixnum))
				       (+ a b)))
			       (value ,(case type
					     (:double '(/ c 2.0d0))
					     (:float '(/ c 2.0))
					     (otherwise '(ash c -1)))))
			  ,form)))))
    (etouq *int16-dispatch*))
  arr)

(defun mono->stereo16 (channel format numcount arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type carray-index numcount))
  (macrolet ((audio-type (type form)
	       `(dotimes (index numcount)
		  (setf (cffi:mem-aref arr :int16 index)
			(let* ((little-index (ash index -1))
			       (value (cffi:mem-aref channel ,type little-index)))
			  ,form)))))
    (etouq *int16-dispatch*))
  arr)

(defun array->int16 (buffer format newlen arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type carray-index newlen))
  (macrolet ((audio-type (type form)
	       `(dotimes (index newlen)
		  (setf (cffi:mem-aref arr :int16 index)
			(let ((value (cffi:mem-aref buffer ,type index)))
			  ,form)))))
    (etouq *int16-dispatch*))
  arr)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *uint8-dispatch*
    '(ecase format
      ((:flt :fltp)
       (let* ((scale 1.0)
	      (scaling-factor (/ 128.0 scale)))
	 (declare (type single-float scale scaling-factor))
	 (audio-type :float
		     (+ 128
			(clamp
			 -128 127
			 (the fixnum
			      (round
			       (* value
				  scaling-factor))))))))
      ((:dbl :dblp)
       (let* ((scale 1.0d0)
	      (scaling-factor (/ 128d0 scale)))
	 (declare (type double-float scale scaling-factor))
	 (audio-type :double
		     (+ 128
			(clamp
			 -128 127
			 (the fixnum
			      (round
			       (*
				value
				scaling-factor))))))))
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

(defun planar-stereo->stereo8 (left right format numcount arr)
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

(defun planar-stereo->mono8 (left right format numcount arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type carray-index numcount))
  (macrolet ((audio-type (type form)
	       `(dotimes (index numcount)
		  (setf (cffi:mem-aref arr :uint8 index)
			(let* ((a (cffi:mem-aref left ,type index))
			       (b (cffi:mem-aref right ,type index))
			       (c (the ,(case type
					      (:double 'double-float)
					      (:float 'single-float)
					      (otherwise 'fixnum))
				       (+ a b)))
			       (value ,(case type
					     (:double '(/ c 2.0d0))
					     (:float '(/ c 2.0))
					     (otherwise '(ash c -1)))))
			  ,form)))))
    (etouq *uint8-dispatch*))
  arr)

(defun interleaved-stereo->mono8 (channel format numcount arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type carray-index numcount))
  (macrolet ((audio-type (type form)
	       `(dotimes (index numcount)
		  (setf (cffi:mem-aref arr :uint8 index)
			(let* ((indexa (ash index 1))
			       (indexb (+ 1 indexa))
			       (a (cffi:mem-aref channel ,type indexa))
			       (b (cffi:mem-aref channel ,type indexb))
			       (c (the ,(case type
					      (:double 'double-float)
					      (:float 'single-float)
					      (otherwise 'fixnum))
				       (+ a b)))
			       (value ,(case type
					     (:double '(/ c 2.0d0))
					     (:float '(/ c 2.0))
					     (otherwise '(ash c -1)))))
			  ,form)))))
    (etouq *uint8-dispatch*))
  arr)

(defun mono->stereo8 (channel format numcount arr)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type carray-index numcount))
  (macrolet ((audio-type (type form)
	       `(dotimes (index numcount)
		  (setf (cffi:mem-aref arr :uint8 index)
			(let* ((little-index (ash index -1))
			       (value (cffi:mem-aref channel ,type little-index)))
			  ,form)))))
    (etouq *uint8-dispatch*))
  arr)

(defun array->uint8 (buffer format newlen arr)
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
