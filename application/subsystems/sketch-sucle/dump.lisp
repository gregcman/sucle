
;;----------------------------------------------------------------------
;package

;;;; package.lisp

(uiop:define-package #:sketch-sucle
    (:use #:cl)
  #+nil ;;FIXME
  (:import-from :kit.sdl2
                :mousebutton-event
                :mousemotion-event
                :mousewheel-event
                :textinput-event
                :keyboard-event
                :other-event
                :close-window)
  (:export :sketch
           :setup
           :draw

           :defsketch

           :sketch-title
           :sketch-width
           :sketch-height
           :sketch-fullscreen
           :sketch-copy-pixels
           :sketch-y-axis

           :title
           :width
           :height
           :fullscreen
           :copy-pixels
           :y-axis

           ;; Math
           :clamp-1
           :normalize

           :+pi+
           :+two-pi+
           :+tau+
           :+half-pi+
           :+quarter-pi+
           :+epsilon+
           :+phi+
           :+golden-ratio+
           :+e+

           :radians
           :degrees

           ;; Utils
           :relative-path

           ;; Colors
           :color
           :make-color
           :color-red
           :color-green
           :color-blue
           :color-hue
           :color-saturation
           :color-brightness
           :color-alpha
           :rgb-to-hsb
           :hsb-to-rgb
           :rgb
           :hsb
           :gray
           :rgb-255
           :hsb-360
           :gray-255
           :hex-to-color
           :color-rgb
           :color-rgba
           :color-hsba
           :color-vector
           :color-vector-255
           :lerp-color
           :random-color
           :hash-color
           :color-filter-grayscale
           :color-filter-invert
           :color-filter-rotate
           :color-filter-hsb
           :+red+
           :+green+
           :+blue+
           :+yellow+
           :+magenta+
           :+cyan+
           :+orange+
           :+white+
           :+black+

           ;; Pen
           :pen
           :pen-stroke
           :pen-fill
           :pen-weight
           :make-pen
           :set-pen
           :copy-pen
           :flip-pen
           :with-pen
           :background

           ;; Shapes
           :point
           :line
           :polyline
           :rect
           :ngon
           :star
           :ellipse
           :circle
           :polygon
           :bezier

           ;; Transforms
           :set-matrix
           :push-matrix
           :pop-matrix
           :translate
           :rotate
           :scale
           :with-matrix
           :with-identity-matrix
           :with-current-matrix

           ;; Channels
           :register-input
           :in
           :out
           :define-channel-observer
           :define-named-channel-observer
           :reset-all-channels

           ;; Figures
           :deffigure

           ;; Resources
           :load-resource
           :image

           ;; Font
           :make-font
           :with-font
           :set-font
           :text
           ))


;;----------------------------------------------------------------------
;math

;;;; math.lisp

(in-package #:sketch-sucle)

;;;  __  __    _  _____ _   _
;;; |  \/  |  / \|_   _| | | |
;;; | |\/| | / _ \ | | | |_| |
;;; | |  | |/ ___ \| | |  _  |
;;; |_|  |_/_/   \_\_| |_| |_|

;; Calculation

(defun clamp-1 (x)
  (alexandria:clamp x 0.0 1.0))

(defun normalize (x low high &key (clamp t) (out-low 0.0) (out-high 1.0))
  (let ((low (min low high))
        (high (max low high))
        (min-out-low (min out-low out-high))
        (min-out-high (max out-low out-high)))
    (let ((norm (+ out-low
                   (* (- out-high out-low)
                      (/ (- x low) (- high low))))))
      (if clamp (alexandria:clamp norm min-out-low min-out-high) norm))))

;; Trigonometry

(defconstant +pi+ PI)
(defconstant +two-pi+ (* PI 2))
(defconstant +tau+ +two-pi+)
(defconstant +half-pi+(/ PI 2))
(defconstant +quarter-pi+ (/ PI 4))
(defconstant +epsilon+ single-float-epsilon)
(defconstant +phi+ 1.61803398875)
(defconstant +golden-ratio+ +phi+)
(defconstant +e+ (exp 1))

(defun radians (deg)
  (* PI (/ deg 180)))

(defun degrees (rad)
  (* 180 (/ rad PI)))


;;----------------------------------------------------------------------
;utils

;;;; utils.lisp

(in-package #:sketch-sucle)

;;;  _   _ _____ ___ _     ____
;;; | | | |_   _|_ _| |   / ___|
;;; | | | | | |  | || |   \___ \
;;; | |_| | | |  | || |___ ___) |
;;;  \___/  |_| |___|_____|____/

(defparameter *build* nil)

(defun pad-list (list pad length)
  (if (>= (length list) length)
      list
      (append (make-list (- length (length list)) :initial-element pad)
              list)))

(defun group (list &optional (group-length 2))
  (flet ((split-n (list n)
           (when (>= (length list) n)
             (loop with acc = '()
                for i below n
                do (setf acc (cons (car list) acc)
                         list (cdr list))
                finally (return (cons (nreverse acc) list))))))
    (loop with acc = '()
       while (or (not acc) (cdr list))
       do (let ((split (split-n list group-length)))
            (when (car split)
              (setf acc (cons (car split) acc)))
            (setf list (cdr split)))
       finally (return (nreverse acc)))))

(defun group-bits (x &optional (bits 8))
  (let ((bit-fill (1- (expt 2 bits))))
    (do* ((x x (ash x (- bits)))
          (acc `(,(boole boole-and x bit-fill))
               (cons (boole boole-and x bit-fill) acc)))
         ((zerop x) (cdr acc)))))

(declaim (inline order-list))
(defun order-list (order list)
  (loop for o in order
     collect (nth o list)))

(declaim (inline mix-lists))
(defun mix-lists (&rest lists)
  (apply #'append (apply #'mapcar #'list lists)))

(declaim (inline div2-inexact))
(defun div2-inexact (a)
  (multiple-value-bind (x y)
      (floor a 2)
    (values x (+ x y))))

(defun abs-or-rel (val src)
  (if (numberp val)
      (cond ((< 0 val 1) (* src val))
            ((<= 1 val) val)
            (t src))
      (or src 0)))

(declaim (inline lerp-list))
(defun lerp-lists (v list-a list-b)
  (mapcar (lambda (a b) (alexandria:lerp v a b)) list-a list-b))

(defun flatten (tree &optional (unless-test (lambda (_) (declare (ignore _)) nil)))
  (let (list)
    (labels ((traverse (subtree)
               (when subtree
                 (if (and (consp subtree) (not (funcall unless-test subtree)))
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree list)))))
      (traverse tree))
    (nreverse list)))

(defun object-to-keyword-hash (object)
  "Expensive operation that turns CL objects into keywords whose names
are MD5 hashes of those objects, stringified. Uniqueness is not guaranteed,
but may be considered unique for all practical purposes."
  (alexandria:make-keyword
   (apply #'alexandria:symbolicate
          (coerce (map 'array (lambda (x) (format nil "~x" x))
                       (md5:md5sum-string (write-to-string object)))
                  'list))))

(defun coerce-float (x)
  (coerce x 'single-float))

(defun copy-buffer (src dst length &key (src-offset 0) (dst-offset 0))
  (declare (optimize (speed 3) (debug 0)))
  (loop for i from 0 below length
     do (setf (cffi:mem-aref dst :uint8 (+ i src-offset))
              (cffi:mem-aref src :uint8 (+ i dst-offset)))))

(defun relative-path (path &optional (system 'sketch-sucle))
  (if *build*
      path
      (format nil "~a" (asdf:system-relative-pathname system path))))


;;----------------------------------------------------------------------
;environment

;;;; environment.lisp

(in-package #:sketch-sucle)

;;;  _____ _   ___     _____ ____   ___  _   _ __  __ _____ _   _ _____
;;; | ____| \ | \ \   / /_ _|  _ \ / _ \| \ | |  \/  | ____| \ | |_   _|
;;; |  _| |  \| |\ \ / / | || |_) | | | |  \| | |\/| |  _| |  \| | | |
;;; | |___| |\  | \ V /  | ||  _ <| |_| | |\  | |  | | |___| |\  | | |
;;; |_____|_| \_|  \_/  |___|_| \_\\___/|_| \_|_|  |_|_____|_| \_| |_|

(defstruct env
  ;; Drawing
  (pen nil)
  (programs nil)
  (model-matrix (sb-cga:identity-matrix))
  (view-matrix nil)
  (matrix-stack nil)
  (y-axis-sgn +1)
  (vao nil)
  (buffer-position 0)
  ;; Typography
  (font nil)
  ;; Textures
  (white-pixel-texture nil)
  (white-color-vector nil)
  ;; Resources
  (resources (make-hash-table))
  ;; Debugging
  (debug-key-pressed nil)
  (red-screen nil))

(defparameter *env* nil)

(defun make-white-pixel-texture ()
  "Sent to shaders when no image is active."
  (let ((texture (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-image-2d :texture-2d 0 :rgba 1 1 0 :bgra :unsigned-byte #(255 255 255 255))
    texture))

(defun set-ortho-matrix (w)
  (with-slots ((env %env) width height y-axis) w
    (setf (env-view-matrix env) (if (eq y-axis :down)
				    (kit.glm:ortho-matrix 0 width height 0 -1 1)
				    (kit.glm:ortho-matrix 0 width 0 height -1 1)))))
(defun set-shader-values (w)
  (with-slots ((env %env) width height y-axis) w
    (let ((program (env-programs env)))
      (kit.gl.shader:use-program program :fill-shader)
      (kit.gl.shader:uniformi program "texid" 0)
      (glhelp::set-active-texture 0)
      (kit.gl.shader:uniform-matrix
       program :view-m 4 (vector (env-view-matrix env))))))

(defun initialize-environment (w)
  (set-ortho-matrix w)
  (with-slots ((env %env) width height y-axis) w
    (setf (env-programs env) (kit.gl.shader:compile-shader-dictionary 'sketch-programs)          
          (env-y-axis-sgn env) (if (eq y-axis :down) +1 -1)
          (env-vao env) (make-instance 'kit.gl.vao:vao :type 'sketch-vao)
          (env-white-pixel-texture env) (make-white-pixel-texture)
          (env-white-color-vector env) #(255 255 255 255)
          (env-pen env) (make-default-pen)
          (env-font env) (make-default-font) ;;FIXME::reenable font
	  ))
  (set-shader-values w))

(defun initialize-gl (w)
  (with-slots ((env %env) width height) w
    ;;(sdl2:gl-set-swap-interval 1) ;;FIXME::remove  -> vsync?
    ;;(setf (kit.sdl2:idle-render w) t) ;;FIXME::remove
    (gl:viewport 0 0 width height)
    (gl:scissor 0 0 width height)
    (gl:enable :blend :line-smooth :polygon-smooth)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:hint :line-smooth-hint :nicest)
    (gl:hint :polygon-smooth-hint :nicest)
    (gl:clear-color 0.0 1.0 0.0 1.0)
    ;;(gl:clear :color-buffer :depth-buffer)
    ;;(gl:flush) ;;FIXME::why is this here?
    ))

(defun debug-mode-p ()
  (and (env-red-screen *env*)
       (env-debug-key-pressed *env*)))

(defun exit-debug-mode ()
  (setf (env-red-screen *env*) nil
        (env-debug-key-pressed *env*) nil))

(defmacro with-environment (env &body body)
  `(let ((*env* ,env))
     ,@body))


;;----------------------------------------------------------------------
;resources

;;;; resources.lisp

(in-package #:sketch-sucle)

;;;  ____  _____ ____   ___  _   _ ____   ____ _____ ____
;;; |  _ \| ____/ ___| / _ \| | | |  _ \ / ___| ____/ ___|
;;; | |_) |  _| \___ \| | | | | | | |_) | |   |  _| \___ \
;;; |  _ <| |___ ___) | |_| | |_| |  _ <| |___| |___ ___) |
;;; |_| \_\_____|____/ \___/ \___/|_| \_\\____|_____|____/

;;; Classes
(defclass resource () ())

(defgeneric free-resource (resource))

(defmethod free-resource :around (resource)
  (when resource
    (call-next-method)))

(defclass image (resource)
  ((texture :accessor image-texture :initarg :texture)
   (width :accessor image-width :initarg :width)
   (height :accessor image-height :initarg :height)))

(defclass typeface (resource)
  ((filename :accessor typeface-filename :initarg :filename)
   (pointer :accessor typeface-pointer :initarg :pointer)))

;;; Loading

(defun load-resource (filename &rest all-keys &key type force-reload-p &allow-other-keys)
  (let ((*env* (or *env* (make-env)))) ;; try faking env if we still don't have one
    (symbol-macrolet ((resource (gethash key (env-resources *env*))))
      (let* ((key (alexandria:make-keyword
		   (alexandria:symbolicate filename (format nil "~a" all-keys)))))
	(when force-reload-p
	  (free-resource resource)
	  (remhash key (env-resources *env*)))
	(when (not resource)
	  (setf resource
		(apply #'load-typed-resource
		       (list*  filename
			       (or type
				   (case (alexandria:make-keyword
					  (alexandria:symbolicate
					   (string-upcase (pathname-type filename))))
				     ((:png
				       ;;:jpg
				       :jpeg
				       ;;:tga
				       :tiff
				       :pnm
				       :gif
				       :bmp)
				      :image)
				     ((:ttf
				       ;;:otf
				       ) :typeface)))
			       all-keys))))
	resource))))

(defgeneric load-typed-resource (filename type &key &allow-other-keys))

(defmethod load-typed-resource (filename type &key &allow-other-keys)
  (if (not type)
      (error (format nil "~a's type cannot be deduced." filename))
      (error (format nil "Unsupported resource type ~a" type))))

(defun make-image-from-surface (surface)
  (let ((texture (glhelp::create-opengl-texture-from-data surface)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:bind-texture :texture-2d 0)
    (make-instance 'image
		   :width (image-utility::image-width surface)
		   :height (image-utility::image-height surface)
		   :texture texture)))

(defmethod load-typed-resource (filename (type (eql :image)) &key &allow-other-keys)
  (make-image-from-surface (image-utility::load-image-from-file filename)))

(defmethod load-typed-resource (filename (type (eql :typeface))
				&key (size 18) &allow-other-keys)
  (make-instance 'typeface
		 :filename filename
		 :pointer (sketch-util::make-font-info
			   :filename filename
			   :size (coerce (truncate size)
					 '(signed-byte 32)))))

(defmethod free-resource ((image image))
  (gl:delete-textures (list (image-texture image))))

(defmethod free-resource ((typeface typeface)))


;;----------------------------------------------------------------------
;color

;;;; color.lisp

(in-package #:sketch-sucle)

;;;   ____ ___  _     ___  ____
;;;  / ___/ _ \| |   / _ \|  _ \
;;; | |  | | | | |  | | | | |_) |
;;; | |__| |_| | |__| |_| |  _ <
;;;  \____\___/|_____\___/|_| \_\

;;; General

(defclass color (resource)
  ((red :initform 0.0 :accessor color-red :initarg :red)
   (green :initform 0.0 :accessor color-green :initarg :green)
   (blue :initform 0.0 :accessor color-blue :initarg :blue)
   (hue :initform 0.0 :accessor color-hue :initarg :hue)
   (saturation :initform 0.0 :accessor color-saturation :initarg :saturation)
   (brightness :initform 0.0 :accessor color-brightness :initarg :brightness)
   (alpha :initform 1.0 :accessor color-alpha :initarg :alpha)))

(defun rgb-to-hsb (r g b)
  (let* ((c-max (max r g b))
         (c-min (min r g b))
         (delta (- c-max c-min))
         (hue (* 60 (cond ((= delta 0) 0)
                          ((= c-max r) (mod (/ (- g b) delta) 6))
                          ((= c-max g) (+ (/ (- b r) delta) 2))
                          ((= c-max b) (+ (/ (- r g) delta) 4))
                          (t 0))))
         (saturation (if (zerop c-max)
                         0
                         (/ delta c-max)))
         (brightness c-max))
    (list (/ hue 360) saturation brightness)))

(defun hsb-to-rgb (h s b)
  (let* ((h (mod (* h 360) 360))
         (c (* b s))
         (x (* c (- 1 (abs (- (mod (/ h 60) 2) 1)))))
         (m (- b c)))
    (mapcar (lambda (x) (+ m x))
            (aref `#((,c ,x 0) (,x ,c 0) (0 ,c ,x)
                     (0 ,x ,c) (,x 0 ,c) (,c 0 ,x))
                  (floor (/ h 60))))))

(defun rgb (red green blue &optional (alpha 1.0))
  (destructuring-bind (red green blue alpha)
      (mapcar #'clamp-1 (list red green blue alpha))
    (let ((hsb (rgb-to-hsb red green blue)))
      (make-instance 'color :red red :green green :blue blue :alpha alpha
                     :hue (elt hsb 0) :saturation (elt hsb 1) :brightness (elt hsb 2)))))

(defun hsb (hue saturation brightness &optional (alpha 1.0))
  (destructuring-bind (hue saturation brightness alpha)
      (mapcar #'clamp-1 (list hue saturation brightness alpha))
    (let ((rgb (hsb-to-rgb hue saturation brightness)))
      (make-instance 'color :hue hue :saturation saturation :brightness brightness :alpha alpha
                     :red (elt rgb 0) :green (elt rgb 1) :blue (elt rgb 2)))))

(defun gray (amount &optional (alpha 1.0))
  (rgb amount amount amount alpha))

(defun rgb-255 (red green blue &optional (alpha 255))
  (rgb (/ red 255) (/ green 255) (/ blue 255) (/ alpha 255)))

(defun hsb-360 (hue saturation brightness &optional (alpha 255))
  (hsb (/ hue 360) (/ saturation 100) (/ brightness 100) (/ alpha 255)))

(defun gray-255 (amount &optional (alpha 255))
  (gray (/ amount 255) (/ alpha 255)))

(defun hex-to-color (string)
  (let ((string (string-left-trim "#" string)))
    (destructuring-bind (r g b &optional (a 1.0))
        (let* ((bits (case (length string)
                       ((3 4) 4)
                       ((6 8) 8)
                       (t (error "~a is not a valid hex color." string))))
               (groups (group-bits (parse-integer string :radix 16 :junk-allowed t)
                                   bits)))
          (pad-list (mapcar (lambda (x) (/ x (if (= bits 4) 15 255))) groups)
                    0
                    (if (= 4 bits)
                        (length string)
                        (/ (length string) 2))))
      (rgb r g b a))))

(defun color-rgb (color)
  (list (color-red color)
        (color-green color)
        (color-blue color)))

(defun color-rgba (color)
  (list (color-red color)
        (color-green color)
        (color-blue color)
        (color-alpha color)))

(defun color-rgba-255 (color)
  (mapcar (lambda (x) (coerce (truncate (* 255 x)) 'unsigned-byte))
          (color-rgba color)))

(defun color-hsba (color)
  (list (color-hue color)
        (color-saturation color)
        (color-brightness color)
        (color-alpha color)))

(defun color-vector (color)
  (apply #'vector (mapcar #'coerce-float (color-rgba color))))

(defun color-vector-255 (color)
  (apply #'vector (color-rgba-255 color)))

;;; Generators

(defun lerp-color (start-color end-color amount &key (mode :hsb))
  (let ((a (clamp-1 amount)))
    (flet ((norm (field)
             (normalize a 0.0 1.0
                        :out-low (slot-value start-color field)
                        :out-high (slot-value end-color field))))
      (if (eq mode :hsb)
          (apply #'hsb (mapcar #'norm '(hue saturation brightness alpha)))
          (apply #'rgb (mapcar #'norm '(red green blue alpha)))))))

(defun random-color (&optional (alpha 1.0))
  (rgb (random 1.0) (random 1.0) (random 1.0) alpha))

(defun hash-color (n &optional (alpha 1.0))
  (let* ((grp (group-bits n))
         (arr (make-array (length grp)
                          :element-type '(unsigned-byte 8)
                          :initial-contents grp))
         (seq (md5:md5sum-sequence arr))
         (hash (loop for i across seq sum i)))
    (hsb-360 (mod (+ (* 144 (mod n 20)) (mod hash 60)) 360)
             (alexandria:clamp (+ 25 (* 25 (mod hash 4)) (mod hash 25)) 0 100)
             (alexandria:clamp (+ 25 (* 25 (mod n 4)) (mod hash 20)) 0 100)
             (* 255 alpha))))

;;; Filters

(defun color-filter-grayscale (color &optional (mode :luminosity))
  (case mode
    ((:lightness 1) (gray (/ (+ (apply #'max (color-rgb color))
                                (apply #'min (color-rgb color))))
                          (color-alpha color)))
    ((:average 2) (gray (/ (apply #'+ (color-rgb color)) 3)
                        (color-alpha color)))
    (t (gray (+ (* 0.21 (color-red color))
                (* 0.72 (color-green color))
                (* 0.07 (color-blue color)))
             (color-alpha color)))))

(defun color-filter-invert (color)
  (hsb (let ((h (- (color-hue color) 0.5)))
         (if (plusp h)
             h
             (+ 1 h)))
       (color-saturation color)
       (color-brightness color)
       (color-alpha color)))

(defun color-filter-rotate (color)
  (rgb (color-green color)
       (color-blue color)
       (color-red color)))

(defun color-filter-hsb (color &key (hue 0.0) (saturation 0.0) (brightness 0.0))
  (let ((hue (clamp-1 (+ hue (color-hue color))))
        (saturation (clamp-1 (+ saturation (color-brightness color))))
        (brightness (clamp-1 (+ brightness (color-brightness color))))
        (alpha (color-alpha color)))
    (destructuring-bind (red green blue) (hsb-to-rgb hue saturation brightness)
      (make-instance 'color
                     :red red :green green :blue blue :alpha alpha
                     :hue hue :saturation saturation :brightness brightness))))

;;; Predefined colors

(defparameter +red+ (rgb 1 0 0))
(defparameter +green+ (rgb 0 1 0))
(defparameter +blue+ (rgb 0 0 1))
(defparameter +yellow+ (rgb 1 1 0))
(defparameter +magenta+ (rgb 1 0 1))
(defparameter +cyan+ (rgb 0 1 1))
(defparameter +orange+ (rgb 1.0 0.5 0.0))
(defparameter +white+ (gray 1))
(defparameter +black+ (gray 0))


;;----------------------------------------------------------------------
;channels

;;;; channels.lisp

(in-package #:sketch-sucle)

;;;   ____ _   _    _    _   _ _   _ _____ _     ____
;;;  / ___| | | |  / \  | \ | | \ | | ____| |   / ___|
;;; | |   | |_| | / _ \ |  \| |  \| |  _| | |   \___ \
;;; | |___|  _  |/ ___ \| |\  | |\  | |___| |___ ___) |
;;;  \____|_| |_/_/   \_\_| \_|_| \_|_____|_____|____/

;;; Channel interface
#+nil;;FIXME::what are channels?
(progn
  (defparameter *channels* (make-hash-table))

  (defun register-input (channel &optional initial (adapter #'identity))
    (unless (assoc adapter (gethash channel *channels*))
      (push (cons adapter initial) (gethash channel *channels*)))
    t)

  (defun in (channel &optional initial (adapter #'identity))
    (register-input channel initial adapter)
    (let ((a (cdr (assoc adapter (gethash channel *channels*)))))
      (or a initial)))

  (defun out-1 (channel message)
    (register-input channel message #'identity)
    (mapcar (lambda (adapter-value-cons)
	      (setf (cdr adapter-value-cons)
		    (funcall (car adapter-value-cons) message)))
	    (gethash channel *channels*))
    (propagate channel))

  (defun out (&rest channel-message)
    (mapcar (lambda (x) (out-1 (first x) (second x)))
	    (group channel-message))
    (values))

  ;;; Channel propagation

  (defstruct propagation
    name
    inputs
    outputs
    function)

  (defparameter *propagations* (make-hash-table))
  (defparameter *channel-propagations* (make-hash-table))

  (defun propagate (channel)
    (mapcar (lambda (p) (funcall (propagation-function p)))
	    (gethash channel *channel-propagations*)))

  (defun find-inputs-and-outputs (body)
    (let ((flat-body (alexandria:flatten body))
	  (inputs-and-outputs (list (list 'in) (list 'out)))
	  (push-into nil))
      (dolist (token flat-body)
	(alexandria:if-let ((io-cons (assoc push-into inputs-and-outputs)))
	  (progn
	    (when (not (member token (cdr io-cons)))
	      (setf (cdr io-cons) (cons token (cdr io-cons))))
	    (setf push-into nil))
	  (setf push-into token)))
      inputs-and-outputs))

  (defun extract-input-registration (body)
    (mapcar (lambda (in-form) (cadr in-form))
	    (remove-if #'atom (flatten body (lambda (x) (eql (car x) 'in))))))

  (defun delete-channel-propagation (channel propagation)
    (setf (gethash channel *channel-propagations*)
	  (remove-if (lambda (x) (eql x propagation))
		     (gethash channel *channel-propagations*))))

  (defun update-propagation-data (name inputs outputs)
    (let ((propagation (gethash name *propagations*)))
      (if propagation
	  (mapcar (lambda (channel)
		    (delete-channel-propagation channel propagation))
		  (propagation-inputs propagation))
	  (setf propagation (make-propagation :name name)
		(gethash name *propagations*) propagation))
      (setf (propagation-inputs propagation) inputs
	    (propagation-outputs propagation) outputs)
      (mapcar (lambda (channel)
		(push propagation (gethash channel *channel-propagations*)))
	      inputs)))

  (defun %define-channel-observer (name body)
    (let ((name (or name (gensym))))
      (let* ((inputs-and-outputs (find-inputs-and-outputs body))
	     (inputs (cdr (assoc 'in inputs-and-outputs)))
	     (outputs (cdr (assoc 'out inputs-and-outputs)))
	     (input-registrations (extract-input-registration body)))
	(update-propagation-data name inputs outputs)
	(mapcar #'register-input input-registrations)
	(setf (propagation-function (gethash name *propagations*))
	      (eval `(lambda () ,@body)))
	(when outputs
	  (mapcar #'propagate inputs)))))

  (defmacro define-named-channel-observer (name &body body)
    (%define-channel-observer name body)
    nil)

  (defmacro define-channel-observer (&body body)
    (%define-channel-observer nil body)
    nil)

  ;;; Utility functions

  (defun reset-channel (channel)
    (remhash channel *channels*)
    (remhash channel *channel-propagations*)
    (maphash (lambda (name propagation)
	       (declare (ignore name))
	       (setf (propagation-inputs propagation)
		     (remove-if (lambda (x) (eql x channel))
				(propagation-inputs propagation))
		     (propagation-outputs propagation)
		     (remove-if (lambda (x) (eql x channel))
				(propagation-outputs propagation))))
	     *propagations*)
    (values))

  (defun reset-all-channels ()
    (setf *channels* (make-hash-table)
	  *propagations* (make-hash-table)
	  *channel-propagations* (make-hash-table))
    (values)))


;;----------------------------------------------------------------------
;shaders

;;;; shaders.lisp

(in-package #:sketch-sucle)

;;;  ____  _   _    _    ____  _____ ____  ____
;;; / ___|| | | |  / \  |  _ \| ____|  _ \/ ___|
;;; \___ \| |_| | / _ \ | | | |  _| | |_) \___ \
;;;  ___) |  _  |/ ___ \| |_| | |___|  _ < ___) |
;;; |____/|_| |_/_/   \_\____/|_____|_| \_\____/

(kit.gl.shader:defdict sketch-programs ()
  (kit.gl.shader:program :fill-shader (:view-m :model-m :texid)
                         (:vertex-shader "
#version 330 core

uniform mat4 model_m;
uniform mat4 view_m;

layout (location = 0) in vec2 vertex;
layout (location = 1) in vec2 texcoord;
layout (location = 2) in vec4 color;

smooth out vec4 f_color;
smooth out vec2 f_texcoord;

void main() {
    gl_Position = view_m * model_m * vec4(vertex, 0.0, 1.0);
    f_texcoord = texcoord;
    f_color = color;
}
")
                         (:fragment-shader "
#version 330 core

uniform sampler2D texid;

smooth in vec4 f_color;
smooth in vec2 f_texcoord;

out vec4 f_out;

void main() {
    f_out = texture(texid, f_texcoord) * f_color;
}
")))


;;----------------------------------------------------------------------
;pen

;;;; pen.lisp

(in-package #:sketch-sucle)

;;;  ____  _____ _   _
;;; |  _ \| ____| \ | |
;;; | |_) |  _| |  \| |
;;; |  __/| |___| |\  |
;;; |_|   |_____|_| \_|

(defstruct pen
  (fill nil)
  (stroke nil)
  (weight 1)
  (curve-steps 100))

(defmacro with-pen (pen &body body)
  (alexandria:once-only
   (pen)
   (alexandria:with-gensyms
    (previous-pen)
    `(let ((,previous-pen (env-pen *env*)))
       (unwind-protect (progn
			 (setf (env-pen *env*) ,pen)
			 ,@body)
	 (setf (env-pen *env*) ,previous-pen))))))

(defun set-pen (pen)
  "Sets environment pen to PEN."
  (setf (env-pen *env*) pen))

(defun flip-pen (pen)
  "Makes a new pen by swapping PEN's fill and stroke colors."
  (make-pen :weight (pen-weight pen)
            :stroke (pen-fill pen)
            :fill (pen-stroke pen)
            :weight (pen-weight pen)
            :curve-steps (pen-curve-steps pen)))

(defun background (color)
  "Fills the sketch window with COLOR."
  (apply #'gl:clear-color (color-rgba color))
  (gl:clear :color-buffer))

(let ((pen))
  (defun make-default-pen ()
    (setf pen (or pen
                  (make-pen :weight 1
                            :fill +white+
                            :stroke +black+)))))


;;----------------------------------------------------------------------
;image

;;;; resources.lisp

(in-package #:sketch-sucle)

;;  ___ __  __    _    ____ _____ ____
;; |_ _|  \/  |  / \  / ___| ____/ ___|
;;  | || |\/| | / _ \| |  _|  _| \___ \
;;  | || |  | |/ ___ \ |_| | |___ ___) |
;; |___|_|  |_/_/   \_\____|_____|____/

(defun image (image-resource x y &optional width height)
  (with-pen (make-pen :fill image-resource
		      :stroke (pen-stroke (env-pen *env*))
		      :weight (pen-weight (env-pen *env*)))
    (rect x
	  y
	  (or (abs-or-rel width (image-width image-resource)))
	  (or (abs-or-rel height (image-height image-resource))))))


;;----------------------------------------------------------------------
;font

;;;; font.lisp

(in-package #:sketch-sucle)

;;;  _____ ___  _   _ _____
;;; |  ___/ _ \| \ | |_   _|
;;; | |_ | | | |  \| | | |
;;; |  _|| |_| | |\  | | |
;;; |_|   \___/|_| \_| |_|

(defclass font (resource)
  ((face :accessor font-face :initarg :face)
   (color :accessor font-color :initarg :color)
   (size :accessor font-size :initarg :size :initform 16)
   (line-height :accessor font-line-height :initarg :line-height :initform 1.41)
   (align :accessor font-align :initarg :align :initform :left)))

(defun make-font (&key face color size line-height align)
  (let* ((*env* (or *env* (make-env))))
    (make-instance 'font
		   :face (or face
			     (font-face (or (env-font *env*)
					    (make-default-font))))
		   :color (or color +black+)
		   :size (or size 18)
		   :line-height (or line-height 1.41)
		   :align (or align :left))))

(defmacro with-font (font &body body)
  (alexandria:once-only
   (font)
   (alexandria:with-gensyms
    (previous-font)			    
    `(let ((,previous-font (env-font *env*)))
       (unwind-protect (progn (setf (env-font *env*) ,font)
			      ,@body)
	 (setf (env-font *env*) ,previous-font))))))

(defun set-font (font)
  (setf (env-font *env*) font))

(defun text-scale (resources spacing width height)
  (let ((rendered-width (apply #'max (mapcar #'image-width resources)))
	(rendered-height (+ (* (- (length resources) 1) spacing)
			    (apply #'+ (mapcar #'image-height resources)))))
    (cond ((and (not (numberp width)) (not (numberp height))) (list 1 1))
	  ((null width) (list 1 (/ height rendered-height)))
	  ((null height) (list (/ width rendered-width) 1))
	  ((eq :keep-ratio width) (list (/ height rendered-height) (/ height rendered-height)))
	  ((eq :keep-ratio height) (list (/ width rendered-width) (/ width rendered-width)))
	  (t (list (/ width rendered-width) (/ height rendered-height))))))

(defun text-align (align width)
  (cond ((eq align :right) (- width))
	((eq align :center) (- (round (/ width 2))))
	(t 0)))

(defun text (text-string x y &optional width height)
  (let* ((font (env-font *env*))
	 (typeface (and font (load-resource (typeface-filename (font-face font))
					    :size (font-size font)))))
    (when (and font (> (length text-string) 0))
      (with-pen (make-pen :stroke nil)
	(destructuring-bind (r g b a) (color-rgba-255 (font-color font))
	  (let* ((top 0)
		 (lines (split-sequence:split-sequence #\newline text-string))
		 (resources (mapcar (lambda (line)
				      (make-image-from-surface
				       (sketch-util::render-text
					(typeface-pointer typeface) line r g b a)))
				    lines))
		 (spacing (* (font-size font) (font-line-height font)))
		 (scale (text-scale resources spacing width height)))
	    (dolist (resource resources)
	      (image resource
		     (+ x (text-align (font-align font) (* (first scale) (image-width resource))))
		     (+ y top)
		     (* (first scale) (image-width resource))
		     (* (second scale) (image-height resource)))
	      (incf top (* (second scale) spacing))
	      (gl:delete-textures (list (image-texture resource))))))))))

(let ((font))
  (defun make-default-font ()
    (setf font (or font
		   (let ((filename (relative-path "res/SourceSansPro-Regular.ttf")))
		     (make-font :face (make-instance 'typeface
						     :filename filename
						     :pointer
						     (sketch-util::make-font-info
						      :filename filename
						      :size 18))
				:color +black+
				:size 18))))))

(let ((font))
  (defun make-error-font ()
    (setf font (or font
		   (let ((filename (relative-path "res/SourceSansPro-Regular.ttf")))
		     (make-font :face (make-instance 'typeface
						     :filename filename
						     :pointer
						     (sketch-util::make-font-info
						      :filename filename
						      :size 16))
				:color +white+
				:size 16))))))


;;----------------------------------------------------------------------
;geometry

;;;; geometry.lisp

(in-package #:sketch-sucle)

;;;   ____ _____ ___  __  __ _____ _____ ______   __
;;;  / ___| ____/ _ \|  \/  | ____|_   _|  _ \ \ / /
;;; | |  _|  _|| | | | |\/| |  _|   | | | |_) \ V /
;;; | |_| | |__| |_| | |  | | |___  | | |  _ < | |
;;;  \____|_____\___/|_|  |_|_____| |_| |_| \_\|_|

(defun edges (vertices &optional (closed t))
  (loop
     for i in (if closed
                  (append (last vertices) (butlast vertices))
                  (butlast vertices))
     for j in (if closed
                  vertices
                  (cdr vertices))
     collect (list i j)))

(defmacro with-lines (lines &body body)
  (flet ((i-to-s (i) (format nil "~a" i)))
    `(symbol-macrolet
         ,(loop
             for line in lines
             for i upfrom 0 by 2
             append
               (loop
                  for sym in '(x x y y)
                  for idx in '(1 2 1 2)
                  for line-accessor in '(caar caadr cadar cadadr)
                  collect
                    `(,(alexandria:symbolicate sym (i-to-s (+ i idx)))
                       (,line-accessor ,line))))
       ,@body)))

(defun translate-line (line d)
  (with-lines (line)
    (let* ((a (atan (- y2 y1) (- x2 x1)))
           (dx (* (sin a) d))
           (dy (* (cos a) d)))
      `((,(+ x1 dx) ,(- y1 dy)) (,(+ x2 dx) ,(- y2 dy))))))

(defun intersect-lines (line1 line2)
  ;; https://en.wikipedia.org/wiki/Line–line_intersection#Given_two_points_on_each_line
  ;; The algorithm is changed so that division by zero never happens.
  ;; The values that are returned for "intersection" may or may not make sense, but
  ;; having responsive but wrong sketch is much better than a red screen.
  (with-lines (line1 line2)
    (let* ((denominator (- (* (- x1 x2) (- y3 y4))
                           (* (- y1 y2) (- x3 x4))))
           (a (if (zerop denominator)
                  (/ (+ x2 x3) 2)
                  (/ (- (* (- (* x1 y2) (* y1 x2)) (- x3 x4))
                        (* (- (* x3 y4) (* y3 x4)) (- x1 x2)))
                     denominator)))
           (b (if (zerop denominator)
                  (/ (+ y2 y3) 2)
                  (/ (- (* (- (* x1 y2) (* y1 x2)) (- y3 y4))
                        (* (- (* x3 y4) (* y3 x4)) (- y1 y2)))
                     denominator))))
      (list a b))))

(defun grow-polygon (polygon d)
  (let ((polygon
         (mapcar (lambda (x) (apply #'intersect-lines x))
                 (edges (mapcar (lambda (x) (translate-line x (- d)))
                                (edges polygon))))))
    (append (cdr polygon) (list (car polygon)))))

(defun triangulate (polygon)
  (mapcar (lambda (point) (list (2d-geometry:x point) (2d-geometry:y point)))
          (apply #'append
                 (mapcar #'2d-geometry:point-list
                         (2d-geometry:decompose-complex-polygon-triangles
                          (apply #'2d-geometry:make-polygon-from-coords polygon))))))

(defun bounding-box (vertices)
  (let ((min-x) (min-y) (max-x) (max-y))
    (dolist (vertex vertices)
      (when (or (not min-x) (< (first vertex) min-x))
        (setf min-x (first vertex)))
      (when (or (not max-x) (> (first vertex) max-x))
        (setf max-x (first vertex)))
      (when (or (not min-y) (< (second vertex) min-y))
        (setf min-y (second vertex)))
      (when (or (not max-y) (> (second vertex) max-y))
        (setf max-y (second vertex))))
    (list (list min-x min-y) (list max-x max-y))))

(defun normalize-to-bounding-box (vertices)
  (let ((box (bounding-box vertices)))
    (with-lines (box)
      (mapcar (lambda (vertex)
                (list (normalize (first vertex) x1 x2)
                      (normalize (second vertex) y1 y2)))
              vertices))))


;;----------------------------------------------------------------------
;drawing

;;;; drawing.lisp

(in-package #:sketch-sucle)

;;;  ____  ____      ___        _____ _   _  ____
;;; |  _ \|  _ \    / \ \      / /_ _| \ | |/ ___|
;;; | | | | |_) |  / _ \ \ /\ / / | ||  \| | |  _
;;; | |_| |  _ <  / ___ \ V  V /  | || |\  | |_| |
;;; |____/|_| \_\/_/   \_\_/\_/  |___|_| \_|\____|
;;;
;;;  http://onrendering.blogspot.com/2011/10/buffer-object-streaming-in-opengl.html
;;;  http://www.java-gaming.org/index.php?topic=32169.0

(kit.gl.vao:defvao sketch-vao ()
  (:interleave ()
               (vertex :float 2)
               (texcoord :float 2)
               (color :unsigned-byte 4 :out-type :float)))

(defparameter *buffer-size* (expt 2 17))
;;(defparameter *vertex-attributes* 5) ;;WTF?
(defparameter *bytes-per-vertex*
  (+
   (* 2 4)
   (* 2 4)
   (* 4 1))
  ;;(+ (* 4 *vertex-attributes*))
  )

(defparameter *draw-mode* :gpu)
(defparameter *draw-sequence* nil)

(defun start-draw ()
  #+nil
  (%gl:bind-buffer :array-buffer

		   ;;1
		   (slot-value *instance* '%array-buffer)
		   ) ;;FIXME::cleanup somewhere?
  #+nil
  (%gl:buffer-data :array-buffer *buffer-size* (cffi:null-pointer) :stream-draw)
  ;;FIXME::THE VALUE OF the-vbo DEPENDS ON THE LAYOUT OF kit.gl.vao.vbos
  ;;ORIGINALLY SKETCH ASSUMES THERE IS 1 VBO NAMED 1. THIS LEAD ME TO A WILD GOOSE CHASE
  ;;OF NULL POINTER HUNTING.
  (let ((the-vbo (aref (slot-value (env-vao *env*) 'kit.gl.vao::vbos) 0)))
    (%gl:bind-buffer :array-buffer the-vbo))
  (%gl:buffer-data :array-buffer *buffer-size* (cffi:null-pointer) :stream-draw)
  (setf (env-buffer-position *env*) 0)
  (kit.gl.vao:vao-bind (env-vao *env*)))

(defun end-draw ()
  (%gl:bind-buffer :array-buffer 0)
  (kit.gl.vao:vao-unbind))

(defun shader-color-texture-values (res)
  (typecase res
    (color (values (or (color-vector-255 res) (env-white-color-vector *env*))
                   (env-white-pixel-texture *env*)))
    (image (values (env-white-color-vector *env*)
                   (or (image-texture res) (env-white-pixel-texture *env*))))))

(defun draw-shape (primitive fill-vertices stroke-vertices)
  (declare (optimize (debug 3)))
  (when (and fill-vertices (pen-fill (env-pen *env*)))
    (multiple-value-bind (shader-color shader-texture)
        (shader-color-texture-values (pen-fill (env-pen *env*)))
      (push-vertices fill-vertices
                     shader-color
                     shader-texture
                     primitive
                     *draw-mode*)))
  (when (and stroke-vertices (pen-stroke (env-pen *env*)))
    (multiple-value-bind (shader-color shader-texture)
        (shader-color-texture-values (pen-stroke (env-pen *env*)))
      (let* ((weight (or (pen-weight (env-pen *env*)) 1))
             (mixed (mix-lists stroke-vertices
                               (grow-polygon stroke-vertices weight))))
        (push-vertices (append mixed (list (first mixed) (second mixed)))
                       shader-color
                       shader-texture
                       :triangle-strip
                       *draw-mode*)))))

(defmethod push-vertices (vertices color texture primitive (draw-mode (eql :gpu)))
  (declare (optimize (debug 3)))
  (kit.gl.shader:uniform-matrix (env-programs *env*) :model-m 4
                                (vector (env-model-matrix *env*)))
  (gl:bind-texture :texture-2d texture)
  (symbol-macrolet ((position (env-buffer-position *env*)))
    ;;(print "what1")

    ;;#+nil ;;FIXME::what is this for?
    (when (> (* *bytes-per-vertex* (+ position (length vertices))) *buffer-size*)
      (start-draw))
    (let ((buffer-pointer (%gl:map-buffer-range :array-buffer
						(* position *bytes-per-vertex*)
						(* (length vertices) *bytes-per-vertex*)
						#x22)))
      #+nil
      (if (cffi:null-pointer-p buffer-pointer)
	  (print "wtf"))
      (progn (fill-buffer buffer-pointer vertices color)
	     (gl:unmap-buffer :array-buffer)))
    (gl:draw-arrays primitive position (length vertices))
    ;;(print "what3")
    (setf position (+ position (length vertices)))))

(defmethod push-vertices (vertices color texture primitive (draw-mode (eql :figure)))
  (declare (optimize (debug 3)))
  (let* ((buffer (static-vectors:make-static-vector
                  (* *bytes-per-vertex* (length vertices))
                  :element-type '(unsigned-byte 8)))
         (buffer-pointer (static-vectors:static-vector-pointer buffer)))
    (fill-buffer buffer-pointer vertices color)
    (push (list :primitive primitive
                :pointer buffer-pointer
                :length (length vertices)) *draw-sequence*)))

(defun fill-buffer (buffer-pointer vertices color)
  (loop
     for idx from 0 by 5;;*vertex-attributes* ;;FIXME ;;coincidence that it lines up? float =4 uint8s = 5 floats
     for (x y) in vertices
     for (tx ty) in (normalize-to-bounding-box vertices) ;;WTF?
     do
       (setf (cffi:mem-aref buffer-pointer :float (+ idx 0)) (coerce-float x)
	     (cffi:mem-aref buffer-pointer :float (+ idx 1)) (coerce-float y)
	     (cffi:mem-aref buffer-pointer :float (+ idx 2)) (coerce-float tx)
	     (cffi:mem-aref buffer-pointer :float (+ idx 3)) (coerce-float (* ty (env-y-axis-sgn *env*)))
	     (cffi:mem-aref buffer-pointer :uint8 (+ (* idx 4) 16)) (aref color 0)
	     (cffi:mem-aref buffer-pointer :uint8 (+ (* idx 4) 17)) (aref color 1)
	     (cffi:mem-aref buffer-pointer :uint8 (+ (* idx 4) 18)) (aref color 2)
	     (cffi:mem-aref buffer-pointer :uint8 (+ (* idx 4) 19)) (aref color 3)
	     )))


;;----------------------------------------------------------------------
					;shapes

;;;; shapes.lisp

(in-package #:sketch-sucle)

;;;  ____  _   _    _    ____  _____ ____
;;; / ___|| | | |  / \  |  _ \| ____/ ___|
;;; \___ \| |_| | / _ \ | |_) |  _| \___ \
;;;  ___) |  _  |/ ___ \|  __/| |___ ___) |
;;; |____/|_| |_/_/   \_\_|   |_____|____/

(defun point (x y)
  (declare (type real x y))
  (with-pen (make-pen :fill (pen-stroke (env-pen *env*)))
    (rect x y 1 1)))

(defun make-line (x1 y1 x2 y2)
  (let* ((a (atan (- y2 y1) (- x2 x1)))
         (w (/ (or (pen-weight (env-pen *env*)) 1) 2))
         (dx (* 2 (sin a) w))
         (dy (* 2 (cos a) w))
         (dx+ (floor dx 2))
         (dx- (- dx dx+))
         (dy+ (floor dy 2))
         (dy- (- dy dy+)))
    (lambda ()
      (draw-shape
       :triangle-strip
       `((,(- x1 dx-) ,(+ y1 dy+))
         (,(- x2 dx-) ,(+ y2 dy+))
         (,(+ x1 dx+) ,(- y1 dy-))
         (,(+ x2 dx+) ,(- y2 dy-)))
       nil))))

(defun line (x1 y1 x2 y2)
  (declare (type real x1 y1 x2 y2))
  (with-pen (flip-pen (env-pen *env*))
    (funcall (make-line x1 y1 x2 y2))))

(defun translated-intersects (lines distance)
  (let ((lines (mapcar (lambda (x) (translate-line x distance)) lines)))
    (edges (append (list (caar lines))
                   (mapcar (lambda (x) (apply #'intersect-lines x))
                           (edges lines nil))
                   (cdar (last lines)))
           nil)))

(defun make-polyline (&rest coordinates)
  (multiple-value-bind (d+ d-)
      (div2-inexact (pen-weight (env-pen *env*)))
    (let* ((lines (edges (group coordinates) nil))
           (lefts (translated-intersects lines (+ d+)))
           (rights (translated-intersects lines (- d-))))
      (lambda ()
        (draw-shape
         :triangle-strip
         (mix-lists (apply #'append lefts)
                    (apply #'append rights))
         nil)))))

(defun polyline (&rest coordinates)
  (case (pen-weight (env-pen *env*))
    (nil nil)
    (1 (mapcar (lambda (x) (line (caar x) (cadar x) (caadr x) (cadadr x)))
               (edges (group coordinates) nil)))
    (t (with-pen (flip-pen (env-pen *env*))
         (funcall (apply #'make-polyline coordinates))))))

(defun make-rect (x y w h)
  (if (and (plusp w) (plusp h))
      (lambda ()
        (draw-shape
         :triangle-strip
         `((,x ,(+ y h)) (,x ,y) (,(+ x w) ,(+ y h)) (,(+ x w) ,y))
         `((,x ,y) (,x ,(+ y h)) (,(+ x w) ,(+ y h)) (,(+ x w) ,y))))
      (lambda ())))

(defun rect (x y w h)
  (declare (type real x y w h))
  (funcall (make-rect x y w h)))

(defun ngon-vertices (n cx cy rx ry &optional (angle 0))
  (let* ((angle (radians angle))
         (rx (if (zerop rx) +epsilon+ rx))
         (theta (/ +tau+ n))
         (tangential (tan theta))
         (radial (cos theta))
         (x (* (cos angle) rx))
         (y (* (sin angle) ry))
         (y-mul (/ ry rx))
         (vertices (list)))
    (dotimes (i n)
      (psetf vertices (cons `(,(+ x cx) ,(+ (* y-mul y) cy)) vertices)
             x (* radial (- x (* (- y) tangential)))
             y (* radial (- y (* x tangential)))))
    (nreverse vertices)))

(defun make-ngon (n cx cy rx ry &optional (angle 0))
  (let ((vertices (ngon-vertices n cx cy rx ry angle)))
    (lambda ()
      (draw-shape :triangle-fan vertices vertices))))

(defun ngon (n cx cy rx ry &optional (angle 0))
  (declare (type fixnum n)
           (type real cx cy rx ry angle))
  (funcall (make-ngon n cx cy rx ry angle)))

(defun make-star (n cx cy ra rb &optional (angle 0))
  (let ((vertices (mix-lists (ngon-vertices n cx cy ra ra (+ 90 angle))
                             (ngon-vertices n cx cy rb rb (- (+ 90 angle) (/ 180 n))))))
    (lambda ()
      (draw-shape :triangle-fan vertices vertices))))

(defun star (n cx cy ra rb &optional (angle 0))
  (declare (type fixnum n)
           (type real cx cy ra rb angle))
  (funcall (make-star n cx cy ra rb angle)))

(defun ellipse (cx cy rx ry)
  (declare (type real cx cy rx ry))
  (when (and (not (zerop rx)) (not (zerop ry)))
    (ngon (max 24 (truncate (* 5 (sqrt (/ (+ (abs rx) (abs ry)) 2))))) cx cy (abs rx) (abs ry))))

(defun circle (x y r)
  (declare (type real x y r))
  (when (not (zerop r))
    (ellipse x y (abs r) (abs r))))

(defun make-polygon (&rest coordinates)
  (list
   :triangles
   (triangulate coordinates)
   (group coordinates)))

(defun polygon (&rest coordinates)
  (apply #'draw-shape (apply #'make-polygon coordinates)))

(defun quadratic-bezier-point (v a b c)
  (let* ((d (lerp-lists v a b))
         (e (lerp-lists v b c)))
    (lerp-lists v d e)))

(defun cubic-bezier-point (v a b c d)
  (let* ((e (lerp-lists v a b))
         (f (lerp-lists v b c))
         (g (lerp-lists v c d)))
    (quadratic-bezier-point v e f g)))

(defun bezier (x1 y1 bx1 by1 bx2 by2 x2 y2)
  (declare (type real x1 y1 bx1 by1 bx2 by2 x2 y2))
  (let ((a (list x1 y1))
        (b (list bx1 by1))
        (c (list bx2 by2))
        (d (list x2 y2))
        (cs (max 2 (pen-curve-steps (env-pen *env*)))))
    (apply #'polyline
           (mapcan (lambda (v) (cubic-bezier-point v a b c d))
                   (alexandria:iota (1+ cs) :step (/ 1 cs))))))


;;----------------------------------------------------------------------
;transforms

;;;; transforms.lisp

(in-package #:sketch-sucle)

;;;  _____ ____      _    _   _ ____  _____ ___  ____  __  __ ____
;;; |_   _|  _ \    / \  | \ | / ___||  ___/ _ \|  _ \|  \/  / ___|
;;;   | | | |_) |  / _ \ |  \| \___ \| |_ | | | | |_) | |\/| \___ \
;;;   | | |  _ <  / ___ \| |\  |___) |  _|| |_| |  _ <| |  | |___) |
;;;   |_| |_| \_\/_/   \_\_| \_|____/|_|   \___/|_| \_\_|  |_|____/

(defun set-matrix (matrix)
  (setf (env-model-matrix *env*) matrix))

(defun push-matrix ()
  (push (env-model-matrix *env*) (env-matrix-stack *env*)))

(defun pop-matrix ()
  (setf (env-model-matrix *env*) (pop (env-matrix-stack *env*))))

(defun set-matrix* (matrix)
  (set-matrix (sb-cga:matrix* (env-model-matrix *env*) matrix)))

(defun translate (dx dy)
  (when (or (not (zerop dx)) (not (zerop dy)))
    (set-matrix* (sb-cga::translate* (coerce-float dx) (coerce-float dy) 0.0))))

(defun rotate (angle &optional (cx 0) (cy 0))
  (translate cx cy)
  (set-matrix* (sb-cga::rotate* 0.0 0.0 (coerce-float (radians angle))))
  (translate (- cx) (- cy)))

(defun scale (sx &optional sy (cx 0) (cy 0))
  (translate cx cy)
  (set-matrix* (sb-cga::scale* (coerce-float sx) (coerce-float (or sy sx)) 0.0))
  (translate (- cx) (- cy)))

(defmacro with-matrix (matrix &body body)
  `(progn
     (unwind-protect
	  (progn (push-matrix)
		 (set-matrix ,matrix)
		 ,@body)
       (pop-matrix))))

(defmacro with-identity-matrix (&body body)
  `(with-matrix sb-cga::+identity-matrix+
     ,@body))

(defmacro with-current-matrix (&body body)
  `(with-matrix (env-model-matrix *env*)
     ,@body))


;;----------------------------------------------------------------------
;sketch

;;;; sketch.lisp

(in-package #:sketch-sucle)

;;; "sketch" goes here. Hacks and glory await!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;;     _|_|_|  _|    _|  _|_|_|_|  _|_|_|_|_|    _|_|_|  _|    _|   ;;;
;;;   _|        _|  _|    _|            _|      _|        _|    _|   ;;;
;;;     _|_|    _|_|      _|_|_|        _|      _|        _|_|_|_|   ;;;
;;;         _|  _|  _|    _|            _|      _|        _|    _|   ;;;
;;;   _|_|_|    _|    _|  _|_|_|_|      _|        _|_|_|  _|    _|   ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Sketch class

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *default-slots*
    '((title :initform "Sketch" :reader sketch-title :initarg :title)
      (width :initform 400 :reader sketch-width :initarg :width)
      (height :initform 400 :reader sketch-height :initarg :height)
      (fullscreen :initform nil :reader sketch-fullscreen :initarg :fullscreen)
      (copy-pixels :initform nil :accessor sketch-copy-pixels :initarg :copy-pixels)
      (y-axis :initform :down :reader sketch-y-axis :initarg :y-axis))))

(defmacro define-sketch-class ()
  `(defclass sketch (;kit.sdl2:gl-window;;FIXME
		     )
     ((%env :initform (make-env))
      (%restart :initform t)
      ;;(%array-buffer :initform nil :reader sketch-array-buffer)
      ,@*default-slots*)))

(define-sketch-class)

;;; Non trivial sketch writers


(defgeneric (setf sketch-title) (value instance)
  (:method (value instance)
    (setf (slot-value instance 'title) value)))
(defgeneric (setf sketch-width) (value instance)
  (:method (value instance)
    (setf (slot-value instance 'width) value)))
(defgeneric (setf sketch-height) (value instance)
  (:method (value instance)
    (setf (slot-value instance 'height) value)))
(defgeneric (setf sketch-fullscreen) (value instance)
  (:method (value instance)
    (setf (slot-value instance 'fullscreen) value)))
(defgeneric (setf sketch-y-axis) (value instance)
  (:method (value instance)
    (setf (slot-value instance 'y-axis) value)))

#+nil;;FIXME
(progn
  (defmacro define-sketch-writer (slot &body body)
    `(defmethod (setf ,(alexandria:symbolicate 'sketch- slot)) (value (instance sketch))
       (setf (slot-value instance ',slot) value)
       (let ((win (kit.sdl2:sdl-window instance)))
	 ,@body)))

  (define-sketch-writer title
    (sdl2:set-window-title win (slot-value instance 'title)))

  (define-sketch-writer width
    (sdl2:set-window-size win (slot-value instance 'width)
			  (slot-value instance 'height)))

  (define-sketch-writer height
    (sdl2:set-window-size win (slot-value instance 'width)
			  (slot-value instance 'height)))

  (define-sketch-writer fullscreen
    (sdl2:set-window-fullscreen win (slot-value instance 'fullscreen)))

  (define-sketch-writer y-axis
    (declare (ignore win))
    (with-slots ((env %env) width height y-axis) instance
      (setf (env-view-matrix env)
	    (if (eq y-axis :down)
		(kit.glm:ortho-matrix 0 width height 0 -1 1)
		(kit.glm:ortho-matrix 0 width 0 height -1 1)))
      (kit.gl.shader:uniform-matrix
       (env-programs env) :view-m 4 (vector (env-view-matrix env))))))

;;; Generic functions

(defgeneric prepare (instance &key &allow-other-keys)
  (:documentation "Generated by DEFSKETCH.")
  (:method-combination progn :most-specific-last))

(defgeneric setup (instance &key &allow-other-keys)
  (:documentation "Called before creating the sketch window.")
  (:method ((instance sketch) &key &allow-other-keys) ()))

(defgeneric draw (instance &key &allow-other-keys)
  (:documentation "Called repeatedly after creating the sketch window,
used for drawing, 60fps.")
  (:method ((instance sketch) &key &allow-other-keys) ()))

;;; Initialization

(defparameter *initialized* nil)

(defun initialize-sketch ()
  (unless *initialized*
    (setf *initialized* t)
    #+nil;;FIXME
    (kit.sdl2:init)
    #+nil
    (sdl2-ttf:init) ;;;FIXME::reenable
    #+nil;;FIXME
    (sdl2:in-main-thread ()
      (sdl2:gl-set-attr :multisamplebuffers 1)
      (sdl2:gl-set-attr :multisamplesamples 4)

      (sdl2:gl-set-attr :context-major-version 3)
      (sdl2:gl-set-attr :context-minor-version 3)
      (sdl2:gl-set-attr :context-profile-mask 1))))

#+nil
(defmethod initialize-instance :around ((instance sketch) &key &allow-other-keys)
  (initialize-sketch)
  (call-next-method)
  #+nil;;FIXME
  (kit.sdl2:start))
#+nil
(defmethod initialize-instance :after ((instance sketch) &rest initargs &key &allow-other-keys)
  (initialize-environment instance)
  (apply #'prepare (list* instance initargs))
  (initialize-gl instance))

(defmethod update-instance-for-redefined-class :after
    ((instance sketch) added-slots discarded-slots property-list &rest initargs)
  (declare (ignore added-slots discarded-slots property-list))
  (apply #'prepare (list* instance initargs)))

;;; Rendering

(defmacro gl-catch (error-color &body body)
  `(handler-case
       (progn
         ,@body)
     (error (e)
       (progn
         (background ,error-color)
         (with-font (make-error-font)
           (with-identity-matrix
             (text (format nil "ERROR~%---~%~a~%---~%Click for restarts." e) 20 20)))
         (setf %restart t
               (env-red-screen *env*) t)))))

(defun draw-window (window)
  (start-draw)
  (draw window)
  (end-draw))

#+nil;;FIXME
(defmethod kit.sdl2:render ((instance sketch))
  (render-sketch-instance instance))
(defparameter *instance* nil)
(defun render-sketch-instance (instance)
  (let ((*instance* instance))
    (with-slots (%env %restart width height copy-pixels) instance
      (with-environment %env
	(with-pen (make-default-pen)
	  (with-font (make-default-font)
	    (with-identity-matrix
	      #+nil;;FIXME -> for integration with other opengl apps
	      (unless copy-pixels
		(background (gray 0.4)))
	      ;; Restart sketch on setup and when recovering from an error.

	      (when %restart
		(gl-catch (rgb 1 1 0.3)
		  (setup instance))
		(setf (slot-value instance '%restart) nil))
	      ;; If we're in the debug mode, we exit from it immediately,
	      ;; so that the restarts are shown only once. Afterwards, we
	      ;; continue presenting the user with the red screen, waiting for
	      ;; the error to be fixed, or for the debug key to be pressed again.
	      (if (debug-mode-p)
		  (progn
		    (exit-debug-mode)
		    (draw-window instance))
		  (progn ;;gl-catch (rgb 0.7 0 0)
		    (draw-window instance))))))))))

;;; Default events

#+nil;;FIXME
(progn
  (defmethod kit.sdl2:keyboard-event :before ((instance sketch) state timestamp repeatp keysym)
	     (declare (ignorable timestamp repeatp))
	     (when (and (eql state :keydown)
			(sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape))
	       (kit.sdl2:close-window instance)))

  (defmethod close-window :before ((instance sketch)) ;;FIXME::cleanup resources?
	     (with-environment (slot-value instance '%env)
	       (loop for resource being the hash-values of (env-resources *env*)
		  do (free-resource resource))))

  (defmethod close-window :after ((instance sketch))
	     (when (and *build* (not (kit.sdl2:all-windows)))
	       #+nil
	       (sdl2-ttf:quit) ;;FIXME::reenable
	       (kit.sdl2:quit))))

;;; DEFSKETCH helpers

(defun first-two (list)
  (list (first list) (second list)))

(defun default-slot-p (slot-or-binding)
  (let ((defaults (mapcar #'car *default-slots*)))
    (typecase slot-or-binding
      (list (member (car slot-or-binding) defaults))
      (t (member slot-or-binding defaults)))))

(defun custom-bindings (&optional bindings)
  (remove-if (lambda (binding)
               (member (car binding) (mapcar #'car *default-slots*)))
             bindings))

(defun intern-accessor (name)
  (intern (string (alexandria:symbolicate 'sketch- name)) :sketch-sucle))

(defun binding-accessor (sketch binding)
  (if (default-slot-p binding)
      (intern-accessor (car binding))
      (or (cadr (member :accessor (cddr binding)))
          (alexandria:symbolicate sketch '- (car binding)))))

(defun make-slot-form (sketch binding)
  `(,(car binding)
     :initarg ,(alexandria:make-keyword (car binding))
     :accessor ,(binding-accessor sketch binding)))

;;; DEFSKETCH channels
#+nil ;;FIXME::channels
(progn
  (defun channel-binding-p (binding)
    (and (consp (cadr binding)) (eql 'in (caadr binding))))

  (defun make-channel-observer (sketch binding)
    `(define-channel-observer
       (let ((win (kit.sdl2:last-window)))
	 (when win
	   (setf (,(binding-accessor sketch binding) win) ,(cadr binding))))))

  (defun make-channel-observers (sketch bindings)
    (mapcar (lambda (binding)
	      (when (channel-binding-p binding)
		(make-channel-observer sketch binding)))
	    bindings))

  (defun replace-channels-with-values (bindings)
    (loop for binding in bindings
       collect (list (car binding)
		     (if (channel-binding-p binding)
			 (caddr (cadr binding))
			 (cadr binding))))))

;;; DEFSKETCH bindings

(defun sketch-bindings-to-slots (sketch bindings)
  (mapcar (lambda (x) (make-slot-form sketch x))
          (remove-if (lambda (x)
                       (member (car x) (mapcar #'car *default-slots*)))
                     bindings)))

;;; DEFSKETCH setf instructions

(defun make-window-parameter-setf ()
  `(setf ,@(mapcan (lambda (slot)
                     `((,(intern-accessor (car slot)) instance) ,(car slot)))
                   *default-slots*)))

(defun make-custom-slots-setf (sketch bindings)
  `(setf ,@(mapcan (lambda (binding)
                     `((slot-value instance ',(car binding)) ,(cadr binding)))
                   bindings)))

(defun make-reinitialize-setf ()
  `(setf ,@(mapcan (lambda (slot)
                     `((,(intern-accessor (car slot)) instance)
                       (,(intern-accessor (car slot)) instance)))
                   *default-slots*)))

(defun custom-slots (bindings)
  (loop
     for b in (mapcar #'car bindings)
     if (not (member b *default-slots*))
     collect b))

;;; DEFSKETCH macro

(defmacro defsketch (sketch-name bindings &body body)
  (let ((default-not-overridden
          (remove-if (lambda (x) (find x bindings :key #'car))
                     (mapcar #'car *default-slots*))))
    `(progn
       (defclass ,sketch-name (sketch)
         ,(sketch-bindings-to-slots `,sketch-name bindings))

       ;;FIXME::what are channels for?
       ;;,@(remove-if-not #'identity (make-channel-observers sketch-name bindings))

       (defmethod prepare progn ((instance ,sketch-name) &rest initargs &key &allow-other-keys)
                  (declare (ignorable initargs))
                  (let* (,@(loop for slot in default-not-overridden
                              collect `(,slot (slot-value instance ',slot)))
                         ,@(mapcar (lambda (binding)
                                     (destructuring-bind (name value)
                                         (first-two binding)
                                       (list name (if (default-slot-p name)
                                                      `(if (getf initargs ,(alexandria:make-keyword name))
                                                           (slot-value instance ',name)
                                                           ,value)
                                                      `(or (getf initargs ,(alexandria:make-keyword name)) ,value)))))
                                   ;;(replace-channels-with-values bindings) ;;FIXME::what is channel for?
				   bindings
				   ))
                    (declare (ignorable ,@(mapcar #'car *default-slots*) ,@(custom-slots bindings)))
                    ,(make-window-parameter-setf)
                    ,(make-custom-slots-setf sketch-name (custom-bindings bindings)))
		  #+nil ;;FIXME::where do the buffers go?
		  (setf (slot-value instance '%array-buffer)
			(car (gl:gen-buffers 1)))
                  (setf (env-y-axis-sgn (slot-value instance '%env))
                        (if (eq (slot-value instance 'y-axis) :down) +1 -1)))

       (defmethod draw ((instance ,sketch-name) &key &allow-other-keys)
         (with-accessors ,(mapcar (lambda (x) (list (car x) (intern-accessor (car x))))
                                  *default-slots*) instance
           (with-slots ,(mapcar #'car bindings) instance
             ,@body)))

       (make-instances-obsolete ',sketch-name)

       (find-class ',sketch-name))))


;;----------------------------------------------------------------------
;figures

;;;; figures.lisp

(in-package #:sketch-sucle)

;;;  _____ ___ ____ _   _ ____  _____ ____
;;; |  ___|_ _/ ___| | | |  _ \| ____/ ___|
;;; | |_   | | |  _| | | | |_) |  _| \___ \
;;; |  _|  | | |_| | |_| |  _ <| |___ ___) |
;;; |_|   |___\____|\___/|_| \_\_____|____/

(defclass figure ()
  ((draws :initarg :draws)))

#+nil ;;FIXME::what?
(defmethod draw ((figure figure) &key &allow-other-keys)
  (symbol-macrolet ((position (env-buffer-position *env*)))
    (with-slots (draws) figure
      (kit.gl.shader:uniform-matrix (env-programs *env*) :model-m 4
                                    (vector (env-model-matrix *env*)))
      (gl:bind-texture :texture-2d (env-white-pixel-texture *env*))
      (dolist (draw draws)
        (let ((primitive (getf draw :primitive))
              (pointer (getf draw :pointer))
              (length (getf draw :length)))
          (when (> (* *bytes-per-vertex* (+ position length)) *buffer-size*)
            (start-draw))
          (let ((buffer-pointer
                 (%gl:map-buffer-range :array-buffer
                                       (* position *bytes-per-vertex*)
                                       (* length *bytes-per-vertex*)
                                       #x22)))
            (copy-buffer pointer buffer-pointer (* length *bytes-per-vertex*))
            (%gl:draw-arrays primitive position length)
            (setf position (+ position length))
            (%gl:unmap-buffer :array-buffer)))))))

(defmacro deffigure (name &body body)
  `(let ((*draw-sequence* nil))
     (let ((*env* (make-env))
           (*draw-mode* :figure))
       (with-pen (make-default-pen)
         ,@body))
     (setf *draw-sequence* (nreverse *draw-sequence*))
     (let ((figure (make-instance 'figure :draws *draw-sequence*)))
       (defun ,name (x y)
         (translate x y)
         (draw figure)
         (translate (- x) (- y))))))


;;----------------------------------------------------------------------
;controllers

;;;; controllers.lisp

(in-package #:sketch-sucle)

;;;   ____ ___  _   _ _____ ____   ___  _     _     _____ ____  ____
;;;  / ___/ _ \| \ | |_   _|  _ \ / _ \| |   | |   | ____|  _ \/ ___|
;;; | |  | | | |  \| | | | | |_) | | | | |   | |   |  _| | |_) \___ \
;;; | |__| |_| | |\  | | | |  _ <| |_| | |___| |___| |___|  _ < ___) |
;;;  \____\___/|_| \_| |_| |_| \_\\___/|_____|_____|_____|_| \_\____/

;;; Mouse
#+nil ;;FIXME
(progn
  (defmethod kit.sdl2:mousemotion-event :after ((instance sketch)
						timestamp button-mask x y xrel yrel)
	     (out :mouse (cons x y)
		  :mouse-x x
		  :mouse-y y
		  :mouse-rel (cons xrel yrel)
		  :mouse-xrel xrel
		  :mouse-yrel yrel))

  (defmethod kit.sdl2:mousewheel-event :after ((instance sketch)
					       timestamp x y)
	     (out :mouse-wheel (cons x y)
		  :mouse-wheel-x x
		  :mouse-wheel-y y))

  (defmethod kit.sdl2:mousebutton-event :after ((instance sketch)
						state timestamp button x y)
	     (with-slots (%env) instance
	       (when (env-red-screen %env)
		 (setf (env-debug-key-pressed %env) t)))))

;;; Keyboard
#+nil ;;FIXME
(progn
  (defmethod keyboard-event :after ((instance sketch)
				    state timestamp repeatp keysym)))

