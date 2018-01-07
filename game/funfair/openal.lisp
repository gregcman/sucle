(defpackage #:sound-stuff
  (:use #:cl #:funland
	))
(in-package #:sound-stuff)


#+nil
("/media/imac/Mac 2/Users/gregmanabat/Music/iTunes/iTunes Music/My Little Pony/Unknown Album/At the Gala.mp3"
       "/media/imac/Mac 2/Users/gregmanabat/Music/iTunes/iTunes Music/Maroon 5/Songs About Jane/02 This Love.m4a"
       "/media/imac/Mac 2/Users/gregmanabat/Music/iTunes/iTunes Music/Unknown Artist/Unknown Album/form this way.mp3"
       "/media/imac/Mac 2/Users/gregmanabat/Music/iTunes/iTunes Music/Unknown Artist/Unknown Album/In Search of Diamonds (Minecraft  Music Video).mp3")

#+nil
("/home/terminal256/src/symmetrical-umbrella/sandbox/res/resources/sound3/damage/hit3.ogg"
 "/home/terminal256/src/symmetrical-umbrella/sandbox/res/resources/streaming/cat.ogg"
 "/home/terminal256/src/symmetrical-umbrella/sandbox/res/resources/sound3/portal/portal.ogg"
 "/home/imac/quicklisp/local-projects/symmetrical-umbrella/sandbox/res/resources/sound3/ambient/weather/rain4.ogg"
 
 "/home/imac/Music/Birdy_-_Keeping_Your_Head_Up_Official.mp3")
(defparameter *music*
  "/home/imac/Music/Louis The Child ft. K.Flay - It's Strange [Premiere] (FIFA 16 Soundtrack) -  128kbps.mp3"
  #+nil
  "/media/imac/Mac 2/Users/gregmanabat/Music/iTunes/iTunes Music/Taylor Swift/Red/04 I Knew You Were Trouble.m4a"
  #+nil
  "/home/imac/Music/6PQv-Adele - Hello.mp3")
(progn
  (defparameter dubs nil)
  (defparameter size nil)
  (defparameter rate nil))
(progn
  (defparameter bytes-per-sample nil)
  (defparameter channels nil))

(defparameter audio-format nil)
(defparameter playsize nil)

(defparameter *playback* :stereo16)

(defparameter dubs2 nil)
(defparameter dubs2same nil)
(defun alut-test (&optional (music *music*))
  (reset)
  (multiple-value-prog1
      (setf (values dubs size rate bytes-per-sample channels audio-format)
	    (cl-ffmpeg::get-sound-buff music))

    (progn (setf (values dubs2 playsize)
		 (convert
		  (aref dubs 0)
		  (case (length dubs)
		    (1 (aref dubs 0))
		    (otherwise (aref dubs 1)))
		  size
		  audio-format
		  *playback*))
	   (setf dubs2same nil))))

(defun reset ()
  (unless dubs2same
    (when (cffi::pointerp dubs2)    
      (cffi::foreign-free dubs2)
      (setf dubs2 nil)))
  (map nil
       (lambda (x)
	 (when (cffi::pointerp x)    
	   (cffi::foreign-free x)))
       dubs)
  (setf dubs nil))

#+nil
(defcenum |AVSampleFormat|
  (:none -1)
  :u8
  :s16
  :s32
  :flt
  :fbl

  :u8p
  :s16p
  :s32p
  :fltp
  :dblp
  :s64
  :s64p

  :nb)


(defun alut-hello-world ()
  (alc:make-context-current *alc-context*)
  (when (and *buffer* (cffi::pointerp *buffer*))
    (al:delete-buffer *buffer*))
  (when (and *source* (cffi::pointerp *source*))
    (al:delete-source *source*))
  (setf *source* (al:gen-source))
  (setf *buffer* (al:gen-buffer))
  (let ((source *source*)
	(buffer *buffer*))
    (al:source source :position (vector 0.0 0.0 0.0))
    (al:source source :velocity (vector 0.0 0.0 0.0))
    (al:source source :gain 1.0)
    (al:source source :pitch 1.0)
    (al:listener :gain 1.0)
    (al:listener :position (vector 0.0 0.0 0.0))
    (al:listener :orientation (vector 0.0 1.0 0.0 0.0 1.0 0.0))
    (al:buffer-data buffer *playback* dubs2 playsize rate
		    )
    (al:source source :buffer buffer)
    (al:source-play source)))

(defparameter *source* nil)
(defparameter *buffer* nil)

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
  (open-context))

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
  (case playblack-format
    (:mono8 (convert8 left len format))
    (:mono16 (convert16 left len format))
    (:stereo8 (interleave8 left right len format))
    (:stereo16 (interleave16 left right len format))))


(eval-when (:compile-toplevel)
  (defparameter *int16-dispatch*
    '(case format
      ((:fltp :flt)
       (audio-type :float
	(truncate 
	 (* (the (single-float -1.0 1.0)
		 value)
	    32767.0))))
      ((:dblp :dbl)
       (audio-type :double
	(truncate 
	 (* (the (double-float -1d0 1d0)
		 value)
	    32767.0d0))))
      ((:s16 :s16p)
       (audio-type :int16 value))
      ((:s32 :s32p)
       (audio-type :int32 (ash value -16)))
      ((:u8 :u8p)
       (audio-type :uint8 (ash (- value 128) 8)))
      ((:s64 :s64p)
       (audio-type :int64 (ash (the (signed-byte 64) value) -48)))
      (:nb (error "wtf is nb?")))))

(defun interleave16 (left right length format)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum length))
  (let ((newlen (* length 2)))
    (declare (type fixnum newlen))
    (let ((arr
	   (cffi:foreign-alloc :int16 :count newlen)))
      (macrolet ((audio-type (type form)
		   `(dotimes (index newlen)
		      (setf (cffi:mem-aref arr :int16 index)
			    (let* ((little-index (ash index -1))
				   (value (if (oddp index)
					      (cffi:mem-aref left ,type little-index)		    
					      (cffi:mem-aref right ,type little-index))))
			      ,form)))))
	(etouq *int16-dispatch*))
      (values arr
	      (the fixnum (* newlen 2))))))

(defun convert16 (buffer newlen format)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum newlen))
  (let ((arr
	 (cffi:foreign-alloc :int16 :count newlen)))
    (macrolet ((audio-type (type form)
		 `(dotimes (index newlen)
		    (setf (cffi:mem-aref arr :int16 index)
			  (let ((value (cffi:mem-aref buffer ,type index)))
			    ,form)))))
      	(etouq *int16-dispatch*))
    (values arr
	    (the fixnum (* 2 newlen)))))

(eval-when (:compile-toplevel)
  (defparameter *uint8-dispatch*
    '(case format
      ((:fltp :flt)
       (audio-type :float
	(truncate 
	 (* (the (single-float -1.0 1.0)
		 (+ 1f0 value))
	    127.5))))
      ((:dblp :dbl)
       (audio-type :double
	(truncate 
	 (* (the (double-float -1d0 1d0)
		 (+ 1d0 value))
	    127.5d0))))
      ((:s16 :s16p)
       (audio-type :int16 (+ 128 (ash value -8))))
      ((:s32 :s32p)
       (audio-type :int32 (+ 128 (ash value -24))))
      ((:u8 :u8p)
       (audio-type :uint8 value))
      ((:s64 :s64p)
       (audio-type :int64 (+ 128 (ash (the (signed-byte 64) value) -56))))
      (:nb (error "wtf is nb?")))))

(defun interleave8 (left right length format)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum length))
  (let ((newlen (* length 2)))
    (declare (type fixnum newlen))
    (let ((arr
	   (cffi:foreign-alloc :uint8 :count newlen)))
      (macrolet ((audio-type (type form)
		   `(dotimes (index newlen)
		      (setf (cffi:mem-aref arr :uint8 index)
			    (let* ((little-index (ash index -1))
				   (value (if (oddp index)
					      (cffi:mem-aref left ,type little-index)		    
					      (cffi:mem-aref right ,type little-index))))
			      ,form)))))
	(etouq *uint8-dispatch*))
      (values arr
	      newlen))))

(defun convert8 (buffer newlen format)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum newlen))
  (let ((arr
	 (cffi:foreign-alloc :uint8 :count newlen)))
    (macrolet ((audio-type (type form)
		 `(dotimes (index newlen)
		    (setf (cffi:mem-aref arr :uint8 index)
			  (let ((value (cffi:mem-aref buffer ,type index)))
			    ,form)))))
      	(etouq *uint8-dispatch*))
    (values arr
	    newlen)))
