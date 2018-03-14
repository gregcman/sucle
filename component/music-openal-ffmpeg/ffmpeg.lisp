(defpackage :cl-ffmpeg
  (:use :cl :cffi :utility :cl-ffmpeg-bindings))

(in-package :cl-ffmpeg)

;; initialize all muxers, demuxers and protocols for libavformat
;; (does nothing if called twice during the course of one program execution)
(eval-when (:execute :load-toplevel)
  (av-register-all))

(defclass some-sound ()
  ((rate :initform nil)
   (bytes-per-sample :initform nil)
   (channels :initform nil)
   (audio-format :initform nil)))

(defclass demux-data ()
  ((&format :initform nil)
   (&frame :initform nil)
   (&codec :initform nil)
   (live :initform nil)))

(defun init-demux-data (inst)
  (let ((a (cffi:foreign-alloc :pointer :count 1))
	(b (cffi:foreign-alloc :pointer :count 1))
	(d (cffi:foreign-alloc :pointer :count 1)))
    (with-slots (&format &frame &codec live) inst
      (setf &format a)
      (setf &frame b)
      (setf &codec d)
      (setf live t)))
  inst)

(defun free-demux-data (inst)
  (with-slots (&format &frame &codec live) inst
    ;;clean up
    (progn 
      (av-frame-free &frame)
      (avcodec-close (cffi:mem-ref &codec :pointer))
      (avformat-close-input &format)
      (avformat-free-context (cffi:mem-ref &format :pointer)))
    (cffi:foreign-free &format)
    (cffi:foreign-free &codec)
    (cffi:foreign-free &frame)
    (setf live nil)))

(cffi:defcfun (avformat-close-input "avformat_close_input") :void
  (avformatcontext :pointer))

(defclass music-stuff ()
  ((demuxer :initform nil)
   (packet :initform nil)
   (sound :initform nil)
   (live :initform nil)))

(defun init-music-stuff (music &optional
				 (sound-data (make-instance 'some-sound))
				 (music-data (make-instance 'music-stuff)))
  (let ((newdemuxer (init-demux-data (make-instance 'demux-data)))
	(newpacket (cffi:foreign-alloc (quote (:struct cl-ffmpeg-bindings::|AVPacket|))
				    :count 1)))
    (with-slots (demuxer packet sound live) music-data
      (setf live t)
      (setf packet newpacket)
      (setf demuxer newdemuxer)
      (setf sound sound-data))
    (let ((aborted? t))
      (unwind-protect
	   (progn
	     (let ((wowt (init-demux newdemuxer music sound-data)))	   
	       ;;prepare to read data
	       (av-init-packet newpacket)
	       (when wowt
		 (setf aborted? nil))))
	;;cleanup
	(when aborted?
	  (free-music-stuff music-data)))
      (if aborted?
	  nil
	  music-data))))
(defun free-music-stuff (music-data)
  (with-slots (packet demuxer live) music-data
    (cffi:foreign-free packet)
    (free-demux-data demuxer)
    (setf live nil
	  packet nil
	  demuxer nil)))

(defmacro %get-sound-buff ((rawdata samples channels audio-format rate) music-data &body body)
  (with-gensyms (demuxer sound packet format codec frame flag)
    (once-only (music-data)
      `(let ((,demuxer (slot-value ,music-data 'demuxer))
	     (,sound (slot-value ,music-data 'sound))
	     (,packet (slot-value ,music-data 'packet)))
	 (declare (type cffi:foreign-pointer ,packet))
	 (let ((,format (mem-ref (slot-value ,demuxer '&format) :pointer))
	       (,codec (mem-ref (slot-value ,demuxer '&codec) :pointer))
	       (,frame (mem-ref (slot-value ,demuxer '&frame) :pointer))
	       (,channels (slot-value ,sound 'channels))
	       (,audio-format (slot-value ,sound 'audio-format))
	       (,rate (slot-value ,sound 'rate)))
	   (declare (type (integer 0 8) ,channels))
	   (declare (type cffi:foreign-pointer ,format ,codec ,frame))
	   (let ((,flag nil))
	     (loop
		(tagbody
		   continue
		   (when (< (av-read-frame ,format ,packet) 0)
		     (setf ,flag t)
		     (return))
		   (cffi:with-foreign-object (gotframe :int 1)
		     (when (< (avcodec-decode-audio4 ,codec ,frame gotframe ,packet) 0)
		       (go continue))
		     (when (zerop (mem-ref gotframe :int))
		       (go continue)))
		   (let ((,samples (cffi:foreign-slot-value
				    ,frame
				    (quote (:struct cl-ffmpeg-bindings::|AVFrame|))
				    (quote cl-ffmpeg-bindings::nb_samples)))
			 (,rawdata (cffi:foreign-slot-value
				    ,frame
				    (quote (:struct cl-ffmpeg-bindings::|AVFrame|))
				    (quote cl-ffmpeg-bindings::data))))
		     ,@body)))
	     ,flag))))))


(defun init-demux (demuxer music sound-data)
  (with-slots ((actual-sample-rate rate)
	       bytes-per-sample
	       channels
	       audio-format) sound-data
    (with-slots (&format &frame &codec) demuxer
      (block bye
	(setf (cffi:mem-ref &format :pointer)
	      (avformat-alloc-context))
	(let ((ret (cffi:with-foreign-string (path music)
		     (avformat-open-input
		      &format
		      path
		      (cffi:null-pointer)
		      (cffi:null-pointer)))))
	  (unless (zerop ret)
	    (print "could not open file ~a")
	    (return-from bye)))
	(when (> 0 (avformat-find-stream-info (cffi:mem-ref &format :pointer)
					      (cffi:null-pointer)))
	  (print "could not retrive stream info from file")
	  (return-from bye))
	(let ((stream-index (find-first-audio-stream2 (cffi:mem-ref &format :pointer))))
	  (when (= -1 stream-index)
	    (print "could not retrieve audio stream from file")
	    (return-from bye))
	  (let* ((stream-array (cffi:foreign-slot-value (cffi:mem-ref &format :pointer)
							(quote (:struct |AVFormatContext|))
							(quote cl-ffmpeg-bindings::streams)))
		 (the-stream (mem-aref stream-array
				       :pointer
				       stream-index)))
	    (progn
	    ;;;find and open codec
	      (setf (cffi:mem-ref &codec :pointer)
		    (foreign-slot-value the-stream
					(quote (:struct cl-ffmpeg-bindings::|AVStream|))
					(quote cl-ffmpeg-bindings::codec)))
	      (when (> 0 (avcodec-open2 (cffi:mem-ref &codec :pointer)
					(avcodec-find-decoder
					 (cffi:foreign-slot-value
					  (cffi:mem-ref &codec :pointer)
					  (quote (:struct cl-ffmpeg-bindings::|AVCodecContext|))
					  (quote cl-ffmpeg-bindings::codec_id)))
					(cffi:null-pointer)))
		(print "failed to open decoder for stream number stream-index for file path")
		(return-from bye)))))
	(let ((codec (cffi:mem-ref &codec :pointer)))
	  (cffi:with-foreign-slots ((cl-ffmpeg-bindings::channels
				     cl-ffmpeg-bindings::channel_layout
				     cl-ffmpeg-bindings::sample_rate
				     cl-ffmpeg-bindings::sample_fmt) codec
				    (:struct cl-ffmpeg-bindings::|AVCodecContext|))
	    (setf actual-sample-rate cl-ffmpeg-bindings::sample_rate)
	    (setf bytes-per-sample (cl-ffmpeg-bindings::av-get-bytes-per-sample
				    cl-ffmpeg-bindings::sample_fmt))
	    (setf audio-format cl-ffmpeg-bindings::sample_fmt)
	    (setf channels cl-ffmpeg-bindings::channels)))
	  
	(progn (setf (cffi:mem-ref &frame :pointer) (av-frame-alloc))
	       (when (cffi:null-pointer-p (cffi:mem-ref &frame :pointer))
		 (print "error allocating the frame")
		 (return-from bye))))))
  t)

(defun find-first-audio-stream2 (format)
  (let ((stream-index -1)
;	(audiocount 0)
	(count (cffi:foreign-slot-value format
					(quote (:struct cl-ffmpeg-bindings::|AVFormatContext|))
					(quote cl-ffmpeg-bindings::nb_streams))))
    (block out
      (dotimes (index count)
	(let* ((streams (cffi:foreign-slot-value
			 format
			 (quote (:struct cl-ffmpeg-bindings::|AVFormatContext|))
			 (quote cl-ffmpeg-bindings::streams)))
	       (stream (cffi:mem-aref
			streams
			:pointer))
	       (codec (cffi:foreign-slot-value
		       stream
		       (quote (:struct cl-ffmpeg-bindings::|AVStream|))
		       (quote cl-ffmpeg-bindings::codec)))
	       (codec_type (cffi:foreign-slot-value
			    codec
			    (quote (:struct cl-ffmpeg-bindings::|AVCodecContext|))
			    (quote cl-ffmpeg-bindings::codec_type))))
	  (when (eq :audio codec_type)
					; (incf audiocount)
	    #+nil
	    (cond ((< 1 audiocount)
		   (format t "more than one audio stream?!?!: find-first-audio-stream2 ~a"
			   audiocount))
		  )
	    (setf stream-index index)
	    
	    (return-from out)
	    ))))
    stream-index))


#+nil
(progn
  (defparameter av-ch-front-left 1)
  (defparameter av-ch-front-right 2)
  (defparameter av-ch-front-center 4)
  (defparameter av-sample-fmt-16 1))
#+nil
(defparameter avmedia-type-audio 1)

#+nil
(defun print-struct (yo)
  (let ((odd -1)
	(acc nil)
	(temp nil))
    (dolist (x yo)
      (if (= -1 odd)
	  (push x temp)
	  (progn
	    (push x temp)
	    (push temp acc)
	    (setf temp nil)))
      (setf odd (* -1 odd)))
    (dolist (x acc)
      (princ x)
      (terpri))))


					;   (&swr :pointer)
#+nil
(progn
  ;;prepare resampler
  (setf (mem-ref &swr :pointer) (swr-alloc))
  (setf actual-sample-rate
	(prepare-resampler2 (cffi:mem-ref &swr :pointer)
			    (cffi:mem-ref &codec :pointer)
			    sample-rate))
  (when (zerop (swr-is-initialized (cffi:mem-ref &swr :pointer)))
    (print "resampler-not-properly initialized")
    (return-from bye -1)))
     #+nil
      ((cffi:with-foreign-object (data-pointer :pointer channels)
	 (dotimes (index channels)
	   (setf (cffi:mem-aref data-pointer :pointer index)
		 (cffi:null-pointer))))
					;data-pointer
       #+nil
       (let ((arr (make-array channels)))
	 (dotimes (i channels)
	   (setf (aref arr i) (cffi:mem-aref data-pointer :pointer i)))
	 (setf adubs arr)))

#+nil
(progn
  (let* ((sample-size  (* typesize samples))
	 (total-size (the fixnum (* typesize size)))
	 (newsize (+ sample-size total-size)))

    (dotimes (index channels)
      (symbol-macrolet ((outbuf (cffi:mem-aref data :pointer index)))
	(let ((inbuf (mem-aref rawdata :pointer index)))
	  (setf outbuf
		(realloc outbuf newsize))
	  (memcpy (cffi:inc-pointer outbuf
				    total-size)
		  inbuf
		  sample-size)))))
  
  (incf size samples))
				;(cffi:mem-ref &swr :pointer)

     					;	    (swr-free &swr)
#+nil
(defun prepare-resampler2 (swr codec user-sample-rate)
;;;  (print-struct (cffi:convert-from-foreign codec (quote (:struct cl-ffmpeg-bindings::|AVCodecContext|))))
  (cffi:with-foreign-slots ((cl-ffmpeg-bindings::channels
			     cl-ffmpeg-bindings::channel_layout
			     cl-ffmpeg-bindings::sample_rate
			     cl-ffmpeg-bindings::sample_fmt) codec
			    (:struct cl-ffmpeg-bindings::|AVCodecContext|))
    #+nil
    (progn
      (av-opt-set-int swr "in_channel_count" cl-ffmpeg-bindings::channels 0)
      (av-opt-set-int swr "out_channel_count" 1 0))

    (progn
      (av-opt-set-int swr "in_channel_layout" cl-ffmpeg-bindings::channel_layout 0)
      (av-opt-set-int swr "out_channel_layout"
		      #+nil
		      av-ch-front-center
		     ; #+nil
		      (logior av-ch-front-left
			      av-ch-front-right) 0))
    (av-opt-set-int swr "in_sample_rate" cl-ffmpeg-bindings::sample_rate 0)
    (av-opt-set-int swr "out_sample_rate" (or user-sample-rate
					      (setf user-sample-rate
						    cl-ffmpeg-bindings::sample_rate)) 0)
    (av-opt-set-sample-fmt swr "in_sample_fmt" (cffi:foreign-enum-value
						(quote cl-ffmpeg-bindings::|AVSampleFormat|)
						cl-ffmpeg-bindings::sample_fmt) 0)
    (av-opt-set-sample-fmt swr "out_sample_fmt" (cffi:foreign-enum-value
						 (quote cl-ffmpeg-bindings::|AVSampleFormat|)
						:s16)
			   0)
    )
  (swr-init swr)
  user-sample-rate)

#+nil
(cffi:with-foreign-object (&buffer :pointer)
  (av-samples-alloc
   &buffer
   (cffi:null-pointer)
   2
   samples
   av-sample-fmt-16
   0)

  #+nil
  (swr-convert
   swr
   &buffer
   samples
   rawdata
   samples))
