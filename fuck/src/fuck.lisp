(in-package :fuck)

(defparameter *al-on?* nil)
(defparameter *sandbox-on* t)
(defparameter *aplay-on* nil)

(defun handoff-five ()
  (setf %gl:*gl-get-proc-address* (e:get-proc-address))
  (when *sandbox-on*
    (sandbox::initialization1))
  (when *aplay-on*
    (aplayground::glinnit))
  (unless *al-on?*
    (start-al)
    (setf *al-on?* t))
  (injection3)) 

(defconstant +million+ (expt 10 6))

(defun injection3 ()
  (let ((ticks 0)
	(dt (floor +million+ 60))
	(current-time (fine-time))
	(accumulator 0))

    (when *sandbox-on*
      (initbag))
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
			  (alut-hello-world))
			(when *aplay-on*
			  (aplayground::physics))
			(when *sandbox-on*
			  (sandbox::thunkit))
			
			(incf ticks dt)
			(decf accumulator dt))
		      (return-from later))))
	     (let ((fraction (/ (float accumulator)
				(float dt))))
	       (when *aplay-on*
		 (aplayground::render))
	       (when *sandbox-on*
		 (progn
		   (sandbox::render fraction)
		   (progn
		     (sandbox::bind-shit :lady)
		     (fuck::draw-baggins))))
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

(defun fine-time ()
  (multiple-value-bind (s m) (sb-ext:get-time-of-day)
    (+ (* (expt 10 6) s) m)))

(defun char-read (path)
  (with-open-file (stream path :element-type 'base-char)
    (let* ((len (file-length stream))
	   (data (make-array len :element-type 'base-char)))
      (dotimes (n len)
	(setf (aref data n) (read-char stream)))
      data)))
(defparameter *data* (char-read "/home/imac/Documents/stuff2/file.osgjs"))
(defparameter *data2* (sandbox::byte-read "/home/imac/Documents/stuff2/model_file.bin"))


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
  
  (sandbox::src-image :lady-png "/home/imac/Documents/stuff2/NightFox/nightfox_d_4.png")
  (sandbox::texture-imagery :lady :lady-png)
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

'
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
  (case 3
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
