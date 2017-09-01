(in-package :fuck)

(defparameter *al-on?* nil)
(defparameter *sandbox-on* t)
(defparameter *aplay-on* nil)

(defun handoff-five ()
  (setf %gl:*gl-get-proc-address* (e:get-proc-address))
  (let ((hash aplayground::*stuff*))
    (maphash (lambda (k v)
	       (if (integerp v)
		   (remhash k hash)))
	     hash))
  (when *sandbox-on*
    (sandbox::initialization1))
  (when *aplay-on*
    (aplayground::glinnit))
  (unless *al-on?*
    (start-al)
    (setf *al-on?* t))
  (injection3)) 
(defparameter barbar nil)
(defconstant +million+ (expt 10 6))

(defun injection3 ()
  (let ((ticks 0)
	(dt (floor +million+ 60))
	(current-time (fine-time))
	(accumulator 0))

    (when *sandbox-on*
  ;;    (initbag)
      )
    (loop
       (if window:*status*
	   (return)
	   (let* ((new-time (fine-time))
		  (frame-time (- new-time current-time)))
	     (let ((quarter-million (* 0.25 +million+)))
	       (if (> frame-time quarter-million)
		   (setf frame-time quarter-million)))
	     (setf current-time new-time)
	     (incf accumulator frame-time)
	     (block later
	       (loop
		  (if (>= accumulator dt)
		      (progn
			(window:poll)
			(when (window:key-j-p :u)
			  (al:source *source* :position (list sandbox::*xpos*
							      sandbox::*ypos*
							      sandbox::*zpos*))
			  (alut-hello-world))
			
			(when (and barbar *aplay-on*)
			  (aplayground::physics)
			  )
			(when *sandbox-on*
			  (sandbox::thunkit))
			
			(incf ticks dt)
			(decf accumulator dt))
		      (return-from later))))
	     (let ((fraction (/ (float accumulator)
				(float dt))))
	       (gl:clear
		:color-buffer-bit
		:depth-buffer-bit)
	       (gl:viewport 0 0 e:*width* e:*height*)

	       (when *sandbox-on*
		 (progn
		   (sandbox::render fraction)
		   (progno
		     (gl:bind-texture
		      :texture-2d
		      (aplayground::get-stuff
		       :lady
		       aplayground::*stuff*
		       aplayground::*backup*
		       
		       ))
		     (fuck::draw-baggins))))
	       (when *aplay-on*
		 (aplayground::render)
		 )
	       )
	     (window:update-display))))))

(defparameter *thread* nil)
(defun main3 ()
  (setf *thread*
	(sb-thread:make-thread   
	 (lambda (stdo)
	   (let ((window::*iresizable* t)
		 (window::*iwidth* 256)
		 (window::*iheight* 256)
		 (*standard-output* stdo))
	     
	     (window::wrapper #'handoff-five)))
	 :arguments  (list *standard-output*))))

;;;time in microseconds
(defun fine-time ()
  (multiple-value-bind (s m) (sb-ext:get-time-of-day)
    (+ (* (expt 10 6) s) m)))

(defun fine-time ()
  (/ (%glfw::get-timer-value)
     (/ (%glfw::get-timer-frequency) (expt 10 6))))

(defun char-read (path)
  (with-open-file (stream path :element-type 'base-char)
    (let* ((len (file-length stream))
	   (data (make-array len :element-type 'base-char)))
      (dotimes (n len)
	(setf (aref data n) (read-char stream)))
      data)))
(defparameter *data* (char-read "/home/imac/Documents/stuff2/file.osgjs"))
#+nil(defparameter *data2* (sandbox::byte-read "/home/imac/Documents/stuff2/model_file.bin"))


(fuktard:with-unsafe-speed
  (defun float-array (offset size &optional (data *data2*))
    (declare (type (simple-array (unsigned-byte 8)) data))
    (let ((array (make-array size :element-type 'single-float)))
      (dotimes (x size)
	(let ((base (+ offset (* x 4))))
	  (let ((payload (logior
			  (aref data (+ base 0))
			  (ash (aref data (+ base 1)) 8)
			  (ash (aref data (+ base 2)) 16)
			  (ash (aref data (+ base 3)) 24))))
	    (let ((ans (ieee-floats:decode-float32 payload)))
	      (print ans)
	      (setf (aref array x)
		    ans)))))
      array)))

(defun print-bits (n &optional (stream *standard-output*))
  (terpri)
  (format stream "~17,'0b" n)
  n)

(defun read-varint-uint32 (offset data)
  (declare (type (simple-array (unsigned-byte 8)) data))
  (let ((acc 0)
	(index 0)
	(bit-offset 0))
    (loop
       (let ((byte (aref data (+ index offset))))
	 (setf acc (logior acc (ash (mod byte 128) bit-offset)))
	 (incf index)
	 (unless (logbitp 7 byte)
	   (return))
	 (incf bit-offset 7)))
    (values acc index)))

(defun uint32-array (offset size &optional (data *data2*))
    (declare (type (simple-array (unsigned-byte 8)) data))
    (let ((array (make-array size :element-type '(unsigned-byte 32))))
      (let ((base offset))
	(dotimes (index size)
	  (multiple-value-bind (num bytesize) (read-varint-uint32 base data)
	    (incf base bytesize)
	    (setf (aref array index)
		  num)))
	(values array base))))
(defparameter testx (make-array 2 :element-type '(unsigned-byte 8) :initial-contents
				'(#b10101100 #b00000010)))
#+nil
(defparameter *json-data*
  (with-input-from-string (x *data*)
    (cl-json:decode-json x)))

(defclass vao ()
  ((vbuff :accessor vertex-buffer)
   (ibuff :accessor index-buffer)
   (va :accessor vertex-array)))

(defparameter *yolobaggins* (make-instance 'vao))

(defun reset-bag ()
  (let ((w *yolobaggins*))
    (gl:delete-vertex-arrays (list (vertex-array w)))
    (gl:delete-buffers (list (vertex-buffer w) (index-buffer w))))
  (initbag))

(defun initbag ()
  (aplayground::bornfnc
     :lady-png
     (lambda ()
       (aplayground::flip-image
	(aplayground::load-png 
	 "/home/imac/Documents/stuff2/NightFox/nightfox_d_4.png"))))
    (aplayground::bornfnc
     :lady
     (lambda ()
       (prog1
	   (lovely-shader-and-texture-uploader:pic-texture
	    (aplayground::get-stuff :lady-png aplayground::*stuff*
				    aplayground::*backup*)
	    :rgb)
	 (lovely-shader-and-texture-uploader::apply-tex-params
	  (quote ((:texture-min-filter . :linear)
		  (:texture-mag-filter . :linear)
		  (:texture-wrap-s . :repeat)
		  (:texture-wrap-t . :repeat)))))))
    
    (mostuff *yolobaggins*))

(progno
    (bind-shit :lady)
    (fuck::draw-baggins))

(defun draw-baggins ()
  (let ((w *yolobaggins*))
  ;;  (gl:disable :cull-face :blend)
;;    (gl:polygon-mode :front-and-back :fill)
    (gl:bind-vertex-array (vertex-array w))
    
    ;; This call actually does the rendering. The vertex data comes from
    ;; the currently-bound VAO. If the input array is null, the indices
    ;; will be taken from the element array buffer bound in the current
    ;; VAO.
    (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int)
		      :count (* 3 (array-total-size (gethash "indices" woywoy)))
		      :offset 0)))

(defparameter len (or 600 (length element2)))
(defun unzigzag (x)
  (if (oddp x)
      (/ (1+ x) -2)
      (/ x 2)))

(progno
 (defparameter wow (make-array (ash 1 16)))
 (map nil (lambda (x)
	    (incf (aref wow (+ (ash 1 15) (unzigzag x))))) vertices
	    )

 (defparameter elements (uint32-array 0 83523))
 (defparameter element2 (uint32-array 131524 29142))
 (defparameter vertices (uint32-array 1877112 (* 3 65532))))

(defparameter woywoy (cl-mesh:parse-wavefront-obj "/home/imac/Documents/stuff2/NightFox/NightFox.obj"))

(progno
 (defparameter wow2 (make-array (* 3 (ash 1 16))))
 (map nil (lambda (x)
	    (incf (aref wow2 (+ (ash 1 16) (unzigzag x))))) element2
	    ))

#+nil
((:OSG.*GEOMETRY (:*UNIQUE-+ID+ . 2)
           (:*PRIMITIVE-SET-LIST
            ((:*DRAW-ELEMENTS-U-INT (:*UNIQUE-+ID+ . 12)
              (:*INDICES (:*UNIQUE-+ID+ . 13)
               (:*ARRAY
                (:*UINT-32-*ARRAY (:*FILE . "model_file.bin.gz")
                 (:*SIZE . 83523) (:*OFFSET . 0) (:*ENCODING . "varint")))
               (:*ITEM-SIZE . 1) (:*TYPE . "ELEMENT_ARRAY_BUFFER"))
              (:*MODE . "TRIANGLE_STRIP")))
            ((:*DRAW-ELEMENTS-U-INT (:*UNIQUE-+ID+ . 14)
              (:*INDICES (:*UNIQUE-+ID+ . 15)
               (:*ARRAY
                (:*UINT-32-*ARRAY (:*FILE . "model_file.bin.gz")
                 (:*SIZE . 29142) (:*OFFSET . 131524) (:*ENCODING . "varint")))
               (:*ITEM-SIZE . 1) (:*TYPE . "ELEMENT_ARRAY_BUFFER"))
              (:*MODE . "TRIANGLES"))))
           (:*STATE-SET
            (:OSG.*STATE-SET (:*UNIQUE-+ID+ . 3)
             (:*TEXTURE-ATTRIBUTE-LIST
              (((:OSG.*TEXTURE (:*UNIQUE-+ID+ . 5)
                 (:*FILE
                  . "textures/8ad0fd34f30c445f99436e94a2a5aa6e/dd8cdbbada8c4d41b007a4fa5aa846e8.png")
                 (:*MAG-FILTER . "LINEAR")
                 (:*MIN-FILTER . "LINEAR_MIPMAP_LINEAR") (:*WRAP-S . "REPEAT")
                 (:*WRAP-T . "REPEAT")))))
             (:*USER-DATA-CONTAINER (:*UNIQUE-+ID+ . 4)
              (:*VALUES ((:*NAME . "UniqueID") (:*VALUE . "1"))))))
           (:*USER-DATA-CONTAINER (:*UNIQUE-+ID+ . 6)
            (:*VALUES ((:*NAME . "attributes") (:*VALUE . "55"))
             ((:*NAME . "vertex_bits") (:*VALUE . "16"))
             ((:*NAME . "vertex_mode") (:*VALUE . "3"))
             ((:*NAME . "uv_0_bits") (:*VALUE . "14"))
             ((:*NAME . "uv_0_mode") (:*VALUE . "3"))
             ((:*NAME . "epsilon") (:*VALUE . "0.25"))
             ((:*NAME . "nphi") (:*VALUE . "720"))
             ((:*NAME . "triangle_mode") (:*VALUE . "7"))
             ((:*NAME . "vertex_obits") (:*VALUE . "16"))
             ((:*NAME . "vtx_bbl_x") (:*VALUE . "-11.609"))
             ((:*NAME . "vtx_bbl_y") (:*VALUE . "-60.0309"))
             ((:*NAME . "vtx_bbl_z") (:*VALUE . "-16.6767"))
             ((:*NAME . "vtx_h_x") (:*VALUE . "0.000715756"))
             ((:*NAME . "vtx_h_y") (:*VALUE . "0.00366222"))
             ((:*NAME . "vtx_h_z") (:*VALUE . "0.00102731"))
             ((:*NAME . "uv_0_bbl_x") (:*VALUE . "0.0019"))
             ((:*NAME . "uv_0_bbl_y") (:*VALUE . "0.0444"))
             ((:*NAME . "uv_0_h_x") (:*VALUE . "0.000121609"))
             ((:*NAME . "uv_0_h_y") (:*VALUE . "0.00011642"))))
           (:*VERTEX-ATTRIBUTE-LIST
            (:*COLOR (:*UNIQUE-+ID+ . 9)
             (:*ARRAY
              (:*FLOAT-32-*ARRAY (:*FILE . "model_file.bin.gz")
               (:*SIZE . 65532) (:*OFFSET . 180204)))
             (:*ITEM-SIZE . 4) (:*TYPE . "ARRAY_BUFFER"))
            (:*NORMAL (:*UNIQUE-+ID+ . 8)
             (:*ARRAY
              (:*UINT-32-*ARRAY (:*FILE . "model_file.bin.gz") (:*SIZE . 65532)
               (:*OFFSET . 1228716) (:*ENCODING . "varint")))
             (:*ITEM-SIZE . 2) (:*TYPE . "ARRAY_BUFFER"))
            (:*TANGENT (:*UNIQUE-+ID+ . 11)
             (:*ARRAY
              (:*UINT-32-*ARRAY (:*FILE . "model_file.bin.gz") (:*SIZE . 65532)
               (:*OFFSET . 1466328) (:*ENCODING . "varint")))
             (:*ITEM-SIZE . 2) (:*TYPE . "ARRAY_BUFFER"))
            (:*TEX-COORD-0 (:*UNIQUE-+ID+ . 10)
             (:*ARRAY
              (:*INT-32-*ARRAY (:*FILE . "model_file.bin.gz") (:*SIZE . 65532)
               (:*OFFSET . 1703380) (:*ENCODING . "varint")))
             (:*ITEM-SIZE . 2) (:*TYPE . "ARRAY_BUFFER"))
            (:*VERTEX (:*UNIQUE-+ID+ . 7)
             (:*ARRAY
              (:*INT-32-*ARRAY (:*FILE . "model_file.bin.gz") (:*SIZE . 65532)
               (:*OFFSET . 1877112) (:*ENCODING . "varint")))
             (:*ITEM-SIZE . 3) (:*TYPE . "ARRAY_BUFFER")))))

(defparameter unique-vertices (make-hash-table :test 'equalp))
(defun generate-vertex-hash ()
  (let* ((name 0)
	 (indexes (gethash "indices" woywoy))
	 (len (array-total-size indexes))
	 (hash unique-vertices))
    (flet ((ass (vec)
	     (let ((value (gethash vec hash)))
	       (if value
		   nil
		   (progn (setf (gethash vec hash) name)
			  (incf name))))))
      (dotimes (i len)
	(let ((vec (aref indexes i)))
	  (let ((a (aref vec 0))
		(b (aref vec 1))
		(c (aref vec 2)))

	    (ass a)
	    (ass b)
	    (ass c)))))))
(defparameter vertarray nil)
(defun order-vertices ()
  (let ((hash unique-vertices))
    (let ((array (make-array (hash-table-count hash))))
      (maphash (lambda (k v)
		 (setf (aref array v) k))
	       hash)
      (setf vertarray array))))
(defparameter vertbuf nil)
(defun flatten-vert ()
  (let* ((array vertarray)
	 (len (array-total-size array))
	 (stride 6))
    (let ((buf (make-array (* len stride))))
      (let ((verts (gethash "vertices" woywoy))
	    (uv (gethash "uv" woywoy)))
	(dotimes (index len)
	  (let ((vec (aref array index)))
	    (let ((base (* index stride)))
	      (let ((verts (aref verts (aref vec 0)))
		    (uv (aref uv (aref vec 1))))
		(setf (aref buf (+ base 0)) (aref verts 0))
		(setf (aref buf (+ base 1)) (aref verts 1))
		(setf (aref buf (+ base 2)) (aref verts 2))
		(setf (aref buf (+ base 3)) 1.0)
		(setf (aref buf (+ base 4)) (aref uv 0))
		(setf (aref buf (+ base 5)) (aref uv 1))
		)))))
      (setf vertbuf buf))))

(progn (generate-vertex-hash)
       (order-vertices)
       (flatten-vert))


(defun mostuff (w)
  (let ((buffers (gl:gen-buffers 2)))
    (setf (vertex-buffer w) (elt buffers 0)
	  (index-buffer w) (elt buffers 1)))
  (gl:bind-buffer :array-buffer (vertex-buffer w))
  (let ((verts vertbuf))
    (let ((arr (gl:alloc-gl-array :float (length verts))))
      (dotimes (i (array-total-size verts))
	(setf (gl:glaref arr i) (aref verts i)))
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr)))
  ;; 0 is always reserved as an unbound object.
  (gl:bind-buffer :array-buffer 0)

  ;; An element array buffer stores vertex indices. We fill it in the
  ;; same way as an array buffer.
  (gl:bind-buffer :element-array-buffer (index-buffer w))
  (let ((hash unique-vertices))
    (let* ((indexes (gethash "indices" woywoy))
	   (len (array-total-size indexes)))
      (let ((arr (gl:alloc-gl-array :unsigned-int (* 3 len))))
	(dotimes (i len)
	  (let ((vec (aref indexes i))
		(base (* 3 i)))
	    (let ((a (aref vec 0))
		  (b (aref vec 1))
		  (c (aref vec 2)))
	      (setf (gl:glaref arr base) (gethash a hash))
	      (setf (gl:glaref arr (+ 1 base)) (gethash b hash))
	      (setf (gl:glaref arr (+ 2 base)) (gethash c hash)))))
	
	(gl:buffer-data :element-array-buffer :static-draw arr)
	(gl:free-gl-array arr))))
  (gl:bind-buffer :element-array-buffer 0)

  ;; Vertex array objects manage which vertex attributes are
  ;; associated with which data buffers. 
  (setf (vertex-array w) (gl:gen-vertex-array))
  (gl:bind-vertex-array (vertex-array w))

  ;; To associate our VBO data with this VAO, we bind it, specify
  ;; which vertex attribute we want to associate it with, and specify
  ;; where the data comes from.
  (gl:bind-buffer :array-buffer (vertex-buffer w))
  ;; In this program, we use attribute 0 for position. If you had
  ;; per-vertex normals, you could use a different attribute for those
  ;; as well.
  (gl:enable-vertex-attrib-array 0)
  ;; Using a null pointer as the data source indicates that we want
  ;; the vertex data to come from the currently bound array-buffer.
  (gl:vertex-attrib-pointer 0 4 :float nil (* 4 6) 0)

  (gl:enable-vertex-attrib-array 2)
  (gl:vertex-attrib-pointer 2 2 :float nil (* 4 6) (* 4 4))



  ;; To associate an element array with this VAO, all we need to do is
  ;; bind the element array buffer we want to use.
  (gl:bind-buffer :element-array-buffer (index-buffer w))

  ;; Once we're done, we can unbind the VAO, and rebind it when we want to render it.
  (gl:bind-vertex-array 0))


(defparameter *music*
  (case 4
    (0 "/home/terminal256/src/symmetrical-umbrella/sandbox/res/resources/sound3/damage/hit3.ogg")
    (1 "/home/terminal256/src/symmetrical-umbrella/sandbox/res/resources/streaming/cat.ogg")
    (2 "/home/terminal256/src/symmetrical-umbrella/sandbox/res/resources/sound3/portal/portal.ogg")
    (3 "/home/imac/quicklisp/local-projects/symmetrical-umbrella/sandbox/res/resources/sound3/ambient/weather/rain4.ogg")
    (4 "/home/imac/Music/6PQv-Adele - Hello.mp3")
    (5 "/home/imac/Music/Louis The Child ft. K.Flay - It's Strange [Premiere] (FIFA 16 Soundtrack) -  128kbps.mp3")
    (6 "/home/imac/Music/Birdy_-_Keeping_Your_Head_Up_Official.mp3")))
(progn
  (defparameter dubs nil)
  (defparameter size nil)
  (defparameter ans nil))

(defun test (&optional (music *music*))
  (reset)
  (setf (values dubs size ans)
	(cl-ffmpeg::get-sound-buff music 44100))
  )

(defun reset ()
  (when (cffi::pointerp dubs)
    
    (cffi::foreign-free dubs)
    (setf dubs nil)))

(defun alut-hello-world ()
  (alc:make-context-current *alc-context*)
  
  (let ((source *source*)
	(buffer *buffer*))
    (al:buffer-data buffer :mono16 dubs (* 2 size) 44100)
    (al:source source :buffer buffer)
    (al:source-play source)))

(defparameter *source* (al:gen-source))
(defparameter *buffer* (al:gen-buffer))

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

#+nil
(defmethod print-object :before ((num fixnum) stream)
  (print "yolo" stream))
#+nil
(remove-method
 #'print-object
 (find-method #'print-object '(:before) '(fixnum stream)))



#+nil
(let ((*print-pprint-dispatch* *moar*))
  (set-pprint-dispatch 'integer (lambda (stream object)
				  (print "yolo")
				  (let ((*print-pretty* nil))
				    (princ object stream)))))

#+nil
(let ((objs *object-stream*))
    (when objs      
      (write-char +stx+ stream)
      
      (write-char +nul+ stream)
      (encode-num-char (fill-pointer objs) stream)
      (write-char +etx+ stream)
      (vector-push-extend object objs)))
(defparameter *object-stream* nil)
(defparameter *numeral-start* nil)

(defun make-char-buffer ()
  (make-array 0 :fill-pointer 0 :adjustable t :element-type (quote character)))

(defparameter footrue (make-char-buffer))
(defparameter fooannot0 (make-char-buffer))
(defparameter fooannot1 (make-char-buffer))
(defparameter obj-stream (make-array 0 :fill-pointer 0 :adjustable t))
(defparameter obj-stream2 (make-array 0 :fill-pointer 0 :adjustable t))

(defparameter *differences* nil)
(defparameter *num-info* nil)
(defparameter *extent-info* nil)


(defun encode-num-char (num stream)
  (write num :stream stream :pretty nil :readably t))
(defun pprint-fun-call (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~^~W~^ ~:_~:I~@{~W~^ ~:_~}~:>")
           stream
           list))
(defparameter *original* *print-pprint-dispatch*)
(defparameter *moar* (copy-pprint-dispatch *print-pprint-dispatch*))
(set-pprint-dispatch (quote (cons (member function quote)))
		     (quote pprint-fun-call)
		     1
		     *original*)

(defun pprint-atomic (stream object)
  (let ((objs *object-stream*)
	(nums *numeral-start*))
    (when objs
      (progn
	(pprint-indent :block most-negative-fixnum stream)
	(pprint-newline :fill stream))
      (write-char (aref nums 0) stream)
      (encode-num-char (fill-pointer objs) stream)
      (write-char (aref nums 1) stream)
      (vector-push-extend object objs)
      (let ((func (pprint-dispatch object *original*)))
	(funcall func stream object))
      (write-char (aref nums 2) stream))))
(let ((*print-pprint-dispatch* *moar*))
  (set-pprint-dispatch (quote t)
		       (quote pprint-atomic) 0))
(defun ourprint (object char-buffer object-buffer)
  (let ((*print-pprint-dispatch* *moar*)
	(*object-stream* object-buffer))
    (write object :stream char-buffer)))

(defun compare-annots (&optional (foo0 fooannot0) (foo1 fooannot1))
  (let* ((len (length foo0))
	 (heat-vector (make-array len :element-type (quote (unsigned-byte 2)))))
    (dotimes (index len)
      (let ((a (aref foo0 index))
	    (b (aref foo1 index)))
	(when (char/= a b)
	  (let ((num (case a
		       (#\1 1)
		       (#\2 2)
		       (#\3 3)
		       (otherwise (error "a is not 1 or 2 or 3")))))
	    (setf (aref heat-vector index) num)))))
    heat-vector))

(defparameter *stringer* (make-array 0 :adjustable t :element-type (quote character) :fill-pointer 0))
(defun splitter (&optional (differences *differences*) (text fooannot0) (scratch *Stringer*))
  (let* ((len (length text))
	 (ids (make-array len :initial-element (quote quote)))
	 (stackno nil)
	 (index 0)
	 (max -1))
    (loop
       (when (>= index len)
	 (return))
       (let ((value (aref differences index)))
	 (case value
	   (0 (setf (aref ids index) (car stackno)))
	   (1 (setf (fill-pointer scratch) 0)
	      (loop
		 (incf index)
		 (let ((different? (aref differences index)))
		   (unless (zerop different?)
		     (let* ((numvalue (read-from-string scratch))
			    (payload numvalue))
		       (when (> numvalue max)
			 (setf max numvalue))
		       (push payload stackno)
		       (setf (aref ids index) (cons :start payload)))
		     (return))
		   (vector-push-extend (aref text index) scratch))))
	   (2 nil)
	   (3 (let ((avalue (pop stackno)))
		(setf (aref ids index) (cons :end avalue))))))
       (incf index))
    (let* ((objs-count (1+ max))
	   (obj-extents (make-array objs-count)))
      (dotimes (x objs-count)
	(setf (aref obj-extents x) (cons 0 0)))
      (dotimes (x len)
	(let ((value (aref ids x)))
	  (when (listp value)
	    (case (car value)
	      (:start (setf (car (aref obj-extents (cdr value))) x))
	      (:end (setf (cdr (aref obj-extents (cdr value))) x))))))
      (values
       ids
       obj-extents))))

(defun whitespace-p (char) 
  (or (char= char #\space)
      (char= char #\tab)
      (char= char #\Return)
      (char= char #\linefeed)
      (char= char #\Newline)))

(defun parallel-walk (&optional (text footrue) (walk-info *num-info*)
			(annot fooannot0))
  (let* ((annot-index 0)
	 (text-index 0)
	 (len (length text))
	 (annot-len (length annot))
	 (obj-ids (make-array len :initial-element nil)))
    (labels ((next-annot-char ()
	       (let ((value (aref walk-info annot-index)))
		 (unless (integerp value)
		     (incf annot-index)
		     (next-annot-char)))))
      (loop
	 (when (or (>= text-index len)
		   (>= annot-index annot-len))
	   (return))
	 (next-annot-char)
	 ;;	   (next-annot-alpha)
	 ;;	   (next-text-alpha)
	 (let ((textchar (aref text text-index))
	       (annot-char (aref annot annot-index)))
;;	   (print (list topstack text-index annot-index textchar annot-char))
	   (cond ((char= textchar annot-char)
		  (setf (aref obj-ids text-index)
			annot-index)
		  (incf text-index)
		  (incf annot-index))
		 (t
		  (let ((annot-white (whitespace-p annot-char))
			(text-white (whitespace-p textchar)))
		    (if annot-white
			(if text-white
			    (progn (setf (aref obj-ids text-index)
					 (cons annot-index nil))
				   (incf text-index)
				   (incf annot-index))
			    (incf annot-index))
			(if text-white
			    (incf text-index)
			    (progn (incf annot-index)
 ;;;fail robustly when someone uses a per-line-prefix in pprint-logical-block
				   (error "unequal"))))))))))
    obj-ids))

(defparameter *obj* nil)
(defparameter *more-flat-changes* nil)
(defparameter *info2* nil)
(defun ouprint2 (object)
  (setf *obj* object)
  (setf (fill-pointer obj-stream) 0)
  (setf (fill-pointer obj-stream2) 0)
  (setf (fill-pointer footrue) 0)
  (setf (fill-pointer fooannot0) 0)
  (setf (fill-pointer fooannot1) 0)
  (let ((*print-case* :downcase)
	(*print-readably* t)
	(*read-eval* nil))
    (with-output-to-string (str footrue)
      (write object :stream str))
    (let ((*numeral-start* "123"))
      (with-output-to-string (str fooannot0)
	(ourprint object str obj-stream)))
    (let ((*numeral-start* "456"))
      (with-output-to-string (str fooannot1)
	(ourprint object str obj-stream2))))
  (dotimes (x (length obj-stream))
    (unless (eq (aref obj-stream x)
		(aref obj-stream2 x))
      (error "object-streams unequal")))
  (setf *differences* (compare-annots fooannot0 fooannot1))
  (setf (values *num-info* *extent-info*)
	(splitter *differences* fooannot0))
  (Setf *more-flat-changes* (delistify *num-info* obj-stream))
  (setf *info2* (map (quote vector)
		     (lambda (x y)
		       (if x x y))
		     *more-flat-changes*
		     *num-info*))
  (let ((more-data (parallel-walk footrue *num-info* fooannot0)))
    (values footrue ;;;;the pretty printed text
	    more-data ;;;;mirror of text, where index maps to num-info
	    *num-info* ;;;;maps to the objects and object extent info
	    *extent-info* ;;;;extent info
	    obj-stream;;;;the objects
	    )))

(defun delistify (&optional (info *num-info*) (objs obj-stream))
  (declare (optimize (debug 3)))
  (let* ((len (length info))
	 (stack nil)
	 (id (make-array (length objs) :initial-element nil))
	 (changes (make-array len :initial-element nil)))
    (dotimes (index len)
      (let ((value (aref info index)))
	(when (and value (listp value))
	  (case (car value)
	    (:start
	     (let ((obj-index (cdr value)))
	       ;;	       (print obj-index)
	       (let ((perhaps-list? (aref objs obj-index)))
		 (if (and stack (consp perhaps-list?))
		     (block nil
		       (let ((first-obj-index (or (car stack) (return))))
			 (when (consp (aref objs first-obj-index))
			   (setf (aref id obj-index) first-obj-index)
			   (setf (aref changes index) :start)))) ;;copy if list parent
		     (push obj-index stack))))) ;;push only if there is not a parent list
	    (:end
	     (If (eql (car stack) ;;the stack
		      ;;the ending, no reason to pop if not equal because of above where
		      ;;nested lists are consolidated above
		      (cdr value))
		 (pop stack)
		 (setf (aref changes index) :end)))))
	(when (integerp value)
	  (let ((destroy? (aref id value)))
	    (when destroy?
	      (setf (aref changes index) destroy?))))))
    changes))

(defun eat-parens (&optional (info *info2*) (objs obj-stream) (chars fooannot0))
  (declare (optimize (debug 3)))
  (let* ((len (length info))
	 (eaters (make-array (length objs) :initial-element nil))
	 (eater-last-object (make-array (length objs) :initial-element nil))
	 (max-object -1))
    (dotimes (index len)
      (let ((value (aref info index)))
	(when (and value (listp value))
	  (case (car value)
	    (:start
	     (let ((obj-index (cdr value)))
	       (let ((maybe-list (aref objs obj-index)))
		 (when (and maybe-list (listp maybe-list))
		   (let ((eater (make-pareneater)))
		     (reset-pareneater maybe-list eater)
		     (setf (aref eaters obj-index) eater))))
	       (setf max-object index)))))
	(flet ((feed-em (p c)
		 (feed c p)
		 					;	 #+nil
		 (progn
		   (terpri)
		   (print c)
		   (print (top p))
		   (terpri))))
	  (when (integerp value)
	    (print (aref chars index))
	    (let ((eater (aref eaters value)))
	      (when eater
		(let ((last-feeding-time (aref eater-last-object value)))
		  (when last-feeding-time
		    (when (not (= max-object last-feeding-time))
		      (feed-em eater #\*))))
		(let ((char (aref chars index)))
		  (feed-em eater char))
		(setf (aref eater-last-object value) max-object)))))))))


(defstruct pareneater
  (stack-stack nil)
  (last nil)
  (parent nil)
  (period? nil)
  (parens 0)
  (lastchar #\Space))
;;(defparameter *stack* nil)
(defun stack (p)
  (first (pareneater-stack-stack p)))
(defun (setf stack) (x p)
  (setf (first (pareneater-stack-stack p)) x))
(defun top (p)
  (first (stack p)))
(defun (setf top) (x p)
  (setf (first (stack p)) x))
(defun poop (p)
  (pop (stack p)))
(defun puush (x p)
  (push x (stack p)))

;;(defparameter *stack-stack* nil)
(defun stack-puush (x p)
  (push (list x) (pareneater-stack-stack p)))
(defun stack-poop (p)
  (pop (pareneater-stack-stack p)))

;;(defparameter *last* 'cdr)
;;(defparameter *period?* nil)
;;(defparameter *parens* 0)
(defun anext (char p)
  (symbol-macrolet ((parens (pareneater-parens p))
		    (period? (pareneater-period? p))
		    (last (pareneater-last p))
		    (parent (pareneater-parent p)))
    (block nil
      (case char
	(#\.
	 (setf period? parens)
	 (return))
	(#\(
	 (incf parens)
	 (setf parent (top p))
	 (stack-puush (car (top p)) p)
	 ;;new frame for new list
	 
	 (setf
	  last 'car))
	(#\*
	 (when period?
	   (return))
	 (case last
	   ;;top is the base
	   (car)
	   ;;top is cdr, underneath is base, but need to advance
	   (cdr 
	    (poop p)
	    (setf (top p) (cdr (top p)))))
	 (setf parent (top p))
	 (puush (car (top p)) p)
	 
	 (setf
	  last 'car))
	(#\Space
	 (when period?
	   (return))
	 (case last
	   ;;top is car, under is base
	   (car (poop p))
	   ;;old list goes away, stuff under
	   (cdr (stack-poop p)))
	 (setf parent (top p))
	 (puush (cdr (top p)) p)
	 (setf
	  last 'cdr))
	(#\) 
	 (unless period?      
	   (case last
	     (car
	      (poop p) ;;;top is car, under is base
	      (setf parent (top p))
	      (puush (cdr (top p)) p))
	     (cdr
	      (stack-poop p)
	      (setf parent (top p))
	      (setf (top p) (cdr (top p)))))       
	   (setf
	    last 'cdr))
	 (when (eql period? parens)
	   (setf period? nil))
	 (decf parens))))))


;;(defparameter *last-char* #\Space)
(defun feed-char (x p)
  (anext x p)
  (setf (pareneater-lastchar p) x))


;;wherever there is one whitespace, there can be many
(defun feed (char p)
  (block nil
    (when (whitespace-p (pareneater-lastchar p))
      (when (whitespace-p char)
	;;eat nothing, go home
	;;whitespace often occurs in clumps
	(return))
      ;;(when (char= char #\))) closing parens do not appear on lines of their own
      )
    (when (char= #\( (pareneater-lastchar p))
      (when (whitespace-p char)
	;;eat nothing, go home
	;;whitespace can occur after opening parens, not closing
	(return)))
    (when (alphanumericp char)
      (feed-char #\* p)
      (return))
    (when (whitespace-p char)
      (setf char #\Space))
    (feed-char char p)))

(defun reset-pareneater (list p)
  (setf (pareneater-last p) 'cdr)
  (setf (pareneater-parent p) nil)
  (setf (pareneater-period? p) nil)
  (setf (pareneater-parens p) 0)
  (setf (pareneater-stack-stack p) (list (list (list list))))
  (setf (pareneater-lastchar p) #\Space))

(defparameter *cells*
  (case 1
    (0 (quote ((a) ((b) (c)) (d) ((e)) f (g) (h ((i) j (k) l (m) (n) o)) p)))
    (1 (quote ((a) ((b) . c) (d) ((e)) f (g) (h ((i) j (k) l (m) (n) . o)) . p)))))
(defparameter buf (make-array 0 :fill-pointer t :element-type (quote character)))
"((A) ((B) (C)) (D) ((E)) F (G) (H ((I) J (K) L (M) (N) O)) P)"
(defparameter *paren-eater* (make-pareneater))
(defun ttest (x)
  (Setf *cells* x)
  (progn
    (setf (fill-pointer buf) 0)
    (with-output-to-string (stream buf)
      (write x :stream stream :pretty t))))

(defun print-stack (x)
  (terpri)
  (print x)
  #+nil
  (dolist (a x)
    (print a)))

(defun test (&optional (p *paren-eater*))
  (reset-pareneater *cells* p)
  (flet ((info ()
	   (if t
	       (print (if t (top p)	;
			  (cons (pareneater-last p)
				(pareneater-parent p)))
		      )
	       (print-stack (pareneater-stack-stack p)))))
    (info)
    (dotimes (x (length buf))
      (let ((value (aref buf x)))
					;	(print value)
	
	(feed value p))
      (info))))


(defun random-shit (x)
  (flet ((asym ()
	   (intern (string (code-char (+ 65 (random 26)))))))
    (if (zerop x)
	(asym)
	(cons (case (random 2)
		(0 (random-shit (1- x)))
		(1 (asym)))
	      (case (random 3)
		(0 nil)
		(1 (random-shit (1- x)))
		(2 (asym)))))))
