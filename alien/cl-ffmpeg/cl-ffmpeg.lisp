(defpackage :cl-ffmpeg
  (:use :cl :cffi :funland :cl-ffmpeg-bindings))

(in-package :cl-ffmpeg)

;; initialize all muxers, demuxers and protocols for libavformat
;; (does nothing if called twice during the course of one program execution)
(eval-when (:execute :load-toplevel)
  (av-register-all))

(defclass some-sound ()
  ((dubs :initform nil)
   (size :initform nil)
   
   (rate :initform nil)
   (bytes-per-sample :initform nil)
   (channels :initform nil)
   (audio-format :initform nil)
   
   (playsize :initform nil)
   (dubs2 :initform nil)))

;   (&swr :pointer)
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
      (avformat-free-context (cffi:mem-ref &format :pointer)))
    (cffi:foreign-free &format)
    (cffi:foreign-free &codec)
    (cffi:foreign-free &frame)
    (setf live nil)))


(defun init-demux (demuxer music sound-data)
  (with-slots ((actual-sample-rate rate)
	       bytes-per-sample
	       channels
	       audio-format) sound-data
    (with-slots (&format &frame &codec) demuxer
;;;	  (print 343)
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
	    (return-from bye -1)))
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
		(return-from bye -1)))))
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
		 (return-from bye -1)))))

	))

(defun get-sound-buff (music sound-data)
  (let ((demuxer (init-demux-data (make-instance 'demux-data))))
    (init-demux demuxer music sound-data)
    (with-slots ((adubs dubs)
		 (asize size)
		 channels) sound-data
      (cffi:with-foreign-object (data-pointer :pointer channels)
	(cffi:with-foreign-object (packet (quote (:struct cl-ffmpeg-bindings::|AVPacket|)))
	  ;;prepare to read data
	  (av-init-packet packet)
	  (setf asize
		(iterate-through-frames2
		 demuxer
		 packet
		 data-pointer
		 sound-data)))
	(let ((arr (make-array channels)))
	  (dotimes (i channels)
	    (setf (aref arr i) (cffi:mem-aref data-pointer :pointer i)))
	  (setf adubs arr))))

      					;	    (swr-free &swr)
    (free-demux-data demuxer)))
#+nil
(progn
  (defparameter av-ch-front-left 1)
  (defparameter av-ch-front-right 2)
  (defparameter av-ch-front-center 4)
  (defparameter av-sample-fmt-16 1))

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
(defparameter avmedia-type-audio 1)

(defun find-first-audio-stream2 (format)
  (let ((stream-index -1)
	(audiocount 0)
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
	    (incf audiocount)
	    (if (< 1 audiocount)
		(format t "more than one audio stream?!?!: find-first-audio-stream2 ~a"
			audiocount)
		(setf stream-index index))
	  ;  (return-from out)
	    ))))
    stream-index))

(defun iterate-through-frames2 (demuxer packet data sound-data)
  (declare (type cffi:foreign-pointer packet data))
					; (declare (optimize (speed 3) (safety 0)))
  (with-slots (&format &codec &frame) demuxer
    (let ((format (mem-ref &format :pointer))
	  (codec (mem-ref &codec :pointer))
	  (frame (mem-ref &frame :pointer)))		;(cffi:mem-ref &swr :pointer)
      (declare (type cffi:foreign-pointer format codec frame))
      (with-slots (channels (typesize bytes-per-sample)) sound-data
					;     (print (list channels packet typesize))
	#+nil
	(declare (type (integer 0 8) typesize)
		 (type (integer 0 8) channels))
	(dotimes (index channels)
	  (setf (cffi:mem-aref data :pointer index)
		(cffi:null-pointer)))
	(let ((size 0)
					;	(count 0)
	      )
	  (declare (type fixnum size))
	  (loop
					;	   (print 234234)
	     (progn
					;(incf count)
					;(print count)
	       (when (< (av-read-frame format packet) 0)
		 (return))
	       (cffi:with-foreign-object (gotframe :int 1)
		 (when (< (avcodec-decode-audio4 codec frame gotframe packet) 0)
		   (continue)
					;(return)
		   )
		 (when (zerop (mem-ref gotframe :int))
		   (continue)))
	       (let ((samples (cffi:foreign-slot-value
			       frame
			       (quote (:struct cl-ffmpeg-bindings::|AVFrame|))
			       (quote cl-ffmpeg-bindings::nb_samples)))
		     (rawdata (cffi:foreign-slot-value
			       frame
			       (quote (:struct cl-ffmpeg-bindings::|AVFrame|))
			       (quote cl-ffmpeg-bindings::data))))
		 #+nil
		 (dotimes (index 8)
		   (print (mem-aref
			   (cffi:foreign-slot-value
			    frame
			    (quote (:struct cl-ffmpeg-bindings::|AVFrame|))
			    (quote cl-ffmpeg-bindings::extended_data))
			   :pointer index)))
		 (let* ((sample-size  (* typesize samples))
			(total-size (the fixnum (* typesize size)))
			(newsize (+ sample-size total-size)))

		   (dotimes (index channels)
		     (symbol-macrolet ((outbuf (cffi:mem-aref data :pointer index)))
		       (let ((inbuf (mem-aref rawdata :pointer index)))
			 (setf outbuf
			       (realloc outbuf newsize))
					;		     (print 234234)
			 (memcpy (cffi:inc-pointer outbuf
						   total-size)
				 inbuf
				 sample-size)))))
		 
		 (incf size samples))))
	  size)))))

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
  (dotimes (x 8)
    (print (mem-aref rawdata :pointer x)))


  #+nil
  (swr-convert
   swr
   &buffer
   samples
   rawdata
   samples))
