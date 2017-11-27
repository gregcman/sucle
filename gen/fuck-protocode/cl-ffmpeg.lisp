(defpackage :cl-ffmpeg
  (:use :cl :cffi :fuktard :cl-ffmpeg-bindings))

(in-package :cl-ffmpeg)

(defun get-sound-buff (music sample-rate)
  (block bye
    (let ((adubs nil)
	  (aans nil)
	  (asize -3))
      (cffi:with-foreign-string (path music)
	;; initialize all muxers, demuxers and protocols for libavformat
	;; (does nothing if called twice during the course of one program execution)
	(av-register-all)
	(cffi:with-foreign-objects ((data-pointer :pointer)
				    (an-int :int)
				    (&format :pointer)
				    (&swr :pointer)
				    (&frame :pointer)
				    (&stream :pointer)
				    (&codec :pointer))

	  (setf (cffi:mem-ref &format :pointer)
		(avformat-alloc-context))
;;;	  (print 343)
	  (unless (zerop
		   (avformat-open-input &format
					path
					(cffi:null-pointer)
					(cffi:null-pointer)))
	    (print "could not open file")
	    (return-from bye -1))
	  (when (> 0 (avformat-find-stream-info (cffi:mem-ref &format :pointer)
						(cffi:null-pointer)))
	    (print "could not retrive stream info from file")
	    (return-from bye -1))
	  (let ((stream-index (find-first-audio-stream2 (cffi:mem-ref &format :pointer))))
	    (when (= -1 stream-index)
	      (print "could not retrieve audio stream from file")
	      (return-from bye -1))
	    (let* ((stream-array (cffi:foreign-slot-value (cffi:mem-ref &format :pointer)
							  (quote (:struct |AVFormatContext|))
							  (quote cl-ffmpeg-bindings::streams)))
		   (the-stream (mem-aref stream-array
					 :pointer
					 stream-index)))
	      (setf (cffi:mem-ref &stream :pointer)
		    the-stream)))
	  (progn
	    ;;;find and open codec
	    (setf (cffi:mem-ref &codec :pointer)
		  (foreign-slot-value (cffi:mem-ref &stream :pointer)
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
	      (return-from bye -1)))
	  (progn
	    ;;prepare resampler
	    (setf (mem-ref &swr :pointer) (swr-alloc))
	    (prepare-resampler2 (cffi:mem-ref &swr :pointer)
				(cffi:mem-ref &codec :pointer)
				sample-rate)
	    (when (zerop (swr-is-initialized (cffi:mem-ref &swr :pointer)))
	      (print "resampler-not-properly initialized")
	      (return-from bye -1)))
	  (progn (setf (cffi:mem-ref &frame :pointer) (av-frame-alloc))
		 (when (cffi:null-pointer-p (cffi:mem-ref &frame :pointer))
		   (print "error allocating the frame")
		   (return-from bye -1)))
	  (cffi:with-foreign-object (packet (quote (:struct cl-ffmpeg-bindings::|AVPacket|)))
	    ;;prepare to read data
	    (av-init-packet packet)

	    (setf aans
		  (iterate-through-frames2 
		   (cffi:mem-ref &format :pointer)
		   packet
		   (cffi:mem-ref &codec :pointer)
		   (cffi:mem-ref &frame :pointer)
		   (cffi:mem-ref &swr :pointer)
		   data-pointer
		   an-int)))
	  (progn ;;clean up
	    (av-frame-free &frame)
	    (swr-free &swr)
	    (avcodec-close (cffi:mem-ref &codec :pointer))
	    (avformat-free-context (cffi:mem-ref &format :pointer)))
	  
	  (setf adubs (cffi:mem-ref data-pointer :pointer))
	  (setf asize (cffi:mem-ref an-int :int))))
      (values adubs asize aans))))

(defparameter av-ch-front-left 1)
(defparameter av-ch-front-right 2)
(defparameter av-ch-front-center 4)
(defparameter av-sample-fmt-16 1)

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

(defun prepare-resampler2 (swr codec user-sample-rate)
;;;  (print-struct (cffi:convert-from-foreign codec (quote (:struct cl-ffmpeg-bindings::|AVCodecContext|))))
  (cffi:with-foreign-slots ((cl-ffmpeg-bindings::channels
			     cl-ffmpeg-bindings::channel_layout
			     cl-ffmpeg-bindings::sample_rate
			     cl-ffmpeg-bindings::sample_fmt) codec
			    (:struct cl-ffmpeg-bindings::|AVCodecContext|))
    (av-opt-set-int swr "in_channel_count" cl-ffmpeg-bindings::channels 0)
    (av-opt-set-int swr "out_channel_count" 1 0)
    (av-opt-set-int swr "in_channel_layout" cl-ffmpeg-bindings::channel_layout 0)
    (av-opt-set-int swr "out_channel_layout" av-ch-front-center 0)
    (av-opt-set-int swr "in_sample_rate" cl-ffmpeg-bindings::sample_rate 0)
    (av-opt-set-int swr "out_sample_rate" user-sample-rate 0)
    (av-opt-set-sample-fmt swr "in_sample_fmt" (cffi:foreign-enum-value
						(quote cl-ffmpeg-bindings::|AVSampleFormat|)
						cl-ffmpeg-bindings::sample_fmt) 0)
    (av-opt-set-sample-fmt swr "out_sample_fmt" (cffi:foreign-enum-value
						(quote cl-ffmpeg-bindings::|AVSampleFormat|)
						:s16)
			   0)
    )
  (swr-init swr))
(defparameter avmedia-type-audio 1)

(defun find-first-audio-stream2 (format)
  (let ((stream-index -1)
	(count (cffi:foreign-slot-value format
					(quote (:struct cl-ffmpeg-bindings::|AVFormatContext|))
					(quote cl-ffmpeg-bindings::nb_streams))))
    (block out
      (dotimes (index count)
	(let* ((streams (cffi:foreign-slot-value format
					(quote (:struct cl-ffmpeg-bindings::|AVFormatContext|))
					(quote cl-ffmpeg-bindings::streams)))
	       (stream (cffi:mem-aref
			streams
			:pointer))
	       (codec (cffi:foreign-slot-value stream
					(quote (:struct cl-ffmpeg-bindings::|AVStream|))
					(quote cl-ffmpeg-bindings::codec)))
	       (codec_type (cffi:foreign-slot-value codec
					(quote (:struct cl-ffmpeg-bindings::|AVCodecContext|))
					(quote cl-ffmpeg-bindings::codec_type))))
	  (when (eq :audio codec_type)
	    (setf stream-index index)
	    (return-from out)))))
    stream-index))

(defun iterate-through-frames2 (format packet codec frame swr data size)
  (setf (cffi:mem-ref data :pointer) (cffi:null-pointer))
  (setf (cffi:mem-ref size :int) 0)
  (loop
     (progn
       (unless (>= (av-read-frame format packet) 0)
	 (return))
       (cffi:with-foreign-objects ((gotframe :int))
	 (when (< (avcodec-decode-audio4 codec frame gotframe packet) 0)
	   (return))
	 (when (zerop (mem-ref gotframe :int))
	   (continue))
	 (cffi:with-foreign-object (&buffer :pointer)
	   (av-samples-alloc
	    &buffer
	    (cffi:null-pointer)
	    1
	    (cffi:foreign-slot-value frame
				     (quote (:struct cl-ffmpeg-bindings::|AVFrame|))
				     (quote cl-ffmpeg-bindings::nb_samples))
	    av-sample-fmt-16
	    0)
	   (let ((frame-count
		  (swr-convert swr
			       &buffer
			       (cffi:foreign-slot-value
				frame
				(quote (:struct cl-ffmpeg-bindings::|AVFrame|))
				(quote cl-ffmpeg-bindings::nb_samples))
			       (cffi:foreign-slot-value
				frame
				(quote (:struct cl-ffmpeg-bindings::|AVFrame|))
				(quote cl-ffmpeg-bindings::data))
			       (cffi:foreign-slot-value
				frame
				(quote (:struct cl-ffmpeg-bindings::|AVFrame|))
				(quote cl-ffmpeg-bindings::nb_samples)))))
	     (setf (cffi:mem-ref data :pointer)
		   (realloc (cffi:mem-ref data :pointer)
			    (* (cffi:foreign-type-size :int16)
			       (+ (cffi:mem-ref size :int)
				  (cffi:foreign-slot-value
				   frame
				   (quote (:struct cl-ffmpeg-bindings::|AVFrame|))
				   (quote cl-ffmpeg-bindings::nb_samples))))))
	     (memcpy (cffi:inc-pointer (mem-ref data :pointer) (* (foreign-type-size :int16)
								  (mem-ref size :int)))
		     (mem-ref &buffer :pointer)
		     (* frame-count (foreign-type-size :int16)))
	     (setf (mem-ref size :int)
		   (+ (mem-ref size :int)
		      frame-count))))))))
