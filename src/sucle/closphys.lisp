(defpackage #:physics
  (:use #:cl)
  (:import-from #:utility
                #:dobox
                #:with-vec
                #:floatify
                #:etouq
                #:once-only
                #:dohash
                #:%list
                #:toggle)
  (:export
   :pos
   :pos-old
   :neck-pitch
   :neck-yaw
   :direction
   :jump-p
   :create-player-entity
   :sneak-p
   :fly-p
   :step-physics))

(in-package #:physics)


;;;; Utilities

;; Access nsb-cga:vec through wrapper macros in case the
;; implementation of vectors changes
(deftype vec () 'nsb-cga:vec) 

(defparameter *temp-vec* (nsb-cga:vec 0.0 0.0 0.0))
(defparameter *temp-vec-2* (nsb-cga:vec 0.0 0.0 0.0))

(defmacro modify (fun a &rest rest)
  (once-only (a)
    `(,fun ,a ,a ,@rest)))

(defun vec-magnitude (vec)
  (nsb-cga:vec-length vec))

(defmacro vec-setf (place x y z)
  `(progn
     (setf (vec-x ,place) ,x)
     (setf (vec-y ,place) ,y)
     (setf (vec-z ,place) ,z)))

(defmacro vec-incf (place delta)
  `(modify nsb-cga:%vec+
           ,place
           ,delta))

(defmacro vec-decf (place delta)
  `(modify nsb-cga:%vec-
           ,place
           ,delta))

(defun set-temp-vec (x y z &optional (temp-vec *temp-vec*))
  (with-vec (a b c) (temp-vec symbol-macrolet)
    (setf a x
          b y
          c z))
  temp-vec)

(defmacro vec (x y z)
  `(nsb-cga:vec ,x ,y ,z))

(defmacro vec* (a f &optional (temp-vec '*temp-vec*))
  `(nsb-cga:%vec* ,temp-vec ,a ,f))

(defmacro vec/ (a f &optional (temp-vec '*temp-vec*))
  `(nsb-cga:%vec/ ,temp-vec ,a ,f))

(defmacro vec+ (a f &optional (temp-vec '*temp-vec*))
  `(nsb-cga:%vec+ ,temp-vec ,a ,f))

(defmacro vec- (a f &optional (temp-vec '*temp-vec*))
  `(nsb-cga:%vec- ,temp-vec ,a ,f))

(defun vec-x (vec)
  (aref vec 0))
(defun vec-y (vec)
  (aref vec 1))
(defun vec-z (vec)
  (aref vec 2))
(defun (setf vec-x) (new vec)
  (setf (aref vec 0) new))
(defun (setf vec-y) (new vec)
  (setf (aref vec 1) new))
(defun (setf vec-z) (new vec)
  (setf (aref vec 2) new))


(define-modify-macro *= (&rest args) *)

(define-modify-macro logiorf (&rest args) logior)

;;;; Classes

(defclass has-position ()
  ((position :type vec
             :initarg :pos
             :initform (vec 0.0 0.0 0.0)
             :accessor pos))
  (:documentation "An object with a world position"))

(defclass has-physics (has-position)
  ((position-old :type vec
                 :initarg :pos
                 :accessor pos-old)
   (velocity :type vec
             :initarg :velocity
             :initform (vec 0.0 0.0 0.0)
             :accessor velocity)
   (acceleration :type vec
                 :initform (vec 0.0 0.0 0.0)
                 :accessor acceleration))
  (:documentation "An object that moves through the world"))

(defclass has-mass ()
  ((mass :type float
         :initarg :mass
         :accessor mass))
  (:documentation "An object with a mass"))

(defmethod mass (object) "Default mass of 1 unit" 1)

(defgeneric apply-impulse (object impulse))

(defmethod apply-impulse ((object has-mass) impulse)
  (vec-incf (velocity object)
            (vec* impulse (/ 1.0 (mass object)))))

(defgeneric apply-force (object force dt))

(defmethod apply-force ((object has-mass) force dt)
  (vec-incf (velocity object)
            (vec* force (/ dt (mass object)))))

(defclass has-drag ()
  ((drag-coefficient :type float
                     :initform 0.0003
                     :initarg :drag-coefficient
                     :accessor drag-coefficient))
  (:documentation "An object which experiences drag"))

(defmethod acceleration :around ((entity has-drag))
  "Apply acceleration due to drag before returning acceleration"
  (let* ((velocity (velocity entity))
         (speed (vec-magnitude velocity))
         (k (drag-coefficient entity)))
    (vec- (call-next-method)
          (vec* velocity
                (/ (* k (expt speed 2))
                   (mass entity))))))


(defparameter *default-acceleration-due-to-gravity* (vec 0.0 (* -9.8 2.5) 0.0))
(defclass has-gravity ()
  ((gravity-p :type boolean
              :initform t
              :accessor gravity-p))
  (:documentation "An object whose motion is affected by gravity"))

(defmethod acceleration-due-to-gravity ((object has-gravity))
  *default-acceleration-due-to-gravity*)

;; method on the acceleration accessor function. Doesn't implement the
;; frame of gravity lag, but I'll figure that out later.
(defmethod acceleration :around ((object has-gravity))
  "Apply acceleration due to gravity before returning acceleration"
  (if (gravity-p object)
      (vec+
       (acceleration-due-to-gravity object)
       (call-next-method object)
       *temp-vec-2*)
      (call-next-method object))

  #+nil(if (gravity-p object)
           (vec+ (acceleration-due-to-gravity object)
                 (call-next-method object))
           (call-next-method object)))


(defclass has-aabb ()
  ((aabb :type aabb
         :initarg :aabb
         :accessor aabb))
  (:documentation "An object with an axis alligned bounding box"))

(defun create-aabb (&optional (maxx 1.0) (maxy maxx) (maxz maxx)
		      (minx (- maxx)) (miny (- maxy)) (minz (- maxz)))
  (aabbcc:make-aabb
   :minx minx
   :maxx maxx
   :miny miny
   :maxy maxy
   :minz minz
   :maxz maxz))

(defparameter *block-aabb*
  ;;;;1x1x1 cube
  (create-aabb 1.0 1.0 1.0 0.0 0.0 0.0))

(defun round-to-nearest (x &optional (n (load-time-value (/ 1.0 128.0))))
  (* n (round (/ x n))))
(defparameter *player-aabb*
  (apply #'create-aabb
	 (mapcar 'round-to-nearest	 
		 '(0.3 0.12 0.3 -0.3 -1.5 -0.3))))

(defmacro floatf (&rest args)
  `(progn
     ,@(mapcar (lambda (arg)
		 `(setf ,arg (floatify ,arg)))
	       args)))


(defmethod not-occupied ((ent has-aabb) x y z)
  (let ((aabb (pos-to-block-aabb x y z)))
    (floatf x y z)
    (sucle::mvc 'aabbcc:aabb-not-overlap
                aabb
                x y z
                (aabb ent)
                (sucle::spread
                 ;;position
                 (pos ent)))))

(defun pos-to-block-aabb (x y z)
  (let ((the-block (world:getblock x y z)))
    (block-to-block-aabb the-block)))
(defun block-to-block-aabb (blockid)
  (declare (ignore blockid))
  ;;FIXME :use defmethod on objects?
  *block-aabb*)


(defclass has-world-collision (has-aabb)
  ((world-contact :type (integer 0 63)
                  :initform 0
                  :accessor world-contact)
   (clip-p :type boolean
           :initform t
           :accessor clip-p))
  (:documentation "An object whose motion is interrupted by blocks in
  the world"))

(defmethod world-contact ((entity has-world-collision))
  (if (clip-p entity)
      (slot-value entity 'world-contact)
      #b000000))

(defun on-ground (entity)
  (when (typep entity 'has-world-collision)
    (logtest (world-contact entity) 4)))

;; (defclass point-projectile (has-physics has-drag) ()
;;   (:documentation "An object "))

(defclass entity (has-physics
                  has-drag
                  has-mass
                  has-world-collision
                  has-gravity)
  ())

(defclass living-entity (entity)
  ((neck-yaw :type float
             :initform 0.0
             :accessor neck-yaw)
   (neck-pitch :type float
               :initform 0.0
               :accessor neck-pitch)
   (direction :type (or nil float)
              :initform nil
              :accessor direction)
   (jump-p :type boolean
           :initform nil
           :accessor jump-p)
   (jump-impulse :type vec
                 :initform (vec 0.0 4.0 0.0)
                 :accessor jump-impulse)
   (movement-speed :type float
                   :accessor movement-speed
                   :allocation class)
   (step-power :type float
               :accessor step-power
               :allocation class)))

(defmethod step-power ((entity living-entity))
  (let ((step-power (slot-value entity 'step-power)))
    (if (on-ground entity)
        (if (not (direction entity))
            (+ step-power 2.0)
            step-power)
        (* step-power 0.5))))

(defclass player-entity (living-entity)
  ((aabb :initform *player-aabb*)
   (mass :initform 1)
   (movement-speed :initform 4.317)
   (step-power :initform 4.0)
   (fly-p :type boolean
          :initform nil
          :accessor fly-p)
   (sneak-p :type (member nil 0 1)
            :initform nil
            :accessor sneak-p)))

(defun create-player-entity ()
  (make-instance 'player-entity))

(defmethod step-power :around ((entity player-entity))
  (if (fly-p entity)
      2.0
      #+nil(if (not (direction entity)) 2.0 4.0)
      (call-next-method entity)))

(defmethod movement-speed :around ((entity player-entity))
  (if (fly-p entity)
      (* 4.0 (call-next-method entity))
      (call-next-method entity)))

;;;; Physics system
(defgeneric step-physics (entity dt)
  (:documentation "Entry point to the physics system. Modify ENTITY to
  have experienced DT seconds"))

(defmethod step-physics (entity dt))

(defgeneric target-velocity (entity))

(defmethod step-physics :before ((entity has-world-collision) dt)
  "Before physics is calculated, find how ENTITY is in contact
with the world and store that information in ENTITY"
  (with-vec (x y z) ((pos entity))
    (setf (world-contact entity) (find-blocks-in-contact-with
                                  x y z (aabb entity)))))

(defun find-blocks-in-contact-with (px py pz aabb)
  (let ((acc #b000000))
    (aabbcc:get-blocks-around (px py pz aabb) (mx my mz contact-var)
      (declare (ignorable contact-var))
      (let ((blockid (world:getblock mx my mz)))
	(when (block-data:data blockid :hard)
	  (logiorf acc (aabbcc:aabb-contact px py pz aabb mx my mz
					    (block-to-block-aabb blockid))))))
    acc))

(defmethod step-physics :around ((entity has-physics) dt)
  "Run all physics methods, then step
velocity and positon"
  (setf (pos-old entity) (pos entity))
  (call-next-method entity dt)
  (step-velocity entity (vec* (acceleration entity) dt))
  (step-position entity (vec* (velocity entity) dt)))

(defgeneric apply-jump (entity))
(defmethod apply-jump ((entity living-entity))
  (when (and (jump-p entity)
             (on-ground entity))
    (apply-impulse entity (jump-impulse entity))))
(defmethod apply-jump :around ((entity player-entity))
  (unless (fly-p entity)
    (call-next-method)))

(defmethod step-physics :before ((entity living-entity) dt)
  (apply-jump entity)
  (apply-movement-force entity dt))

;; (defmethod step-physics ((entity player-entity) dt)
;;   (let ((speed (movement-speed entity)))
;;     ))

(defmethod target-velocity ((entity player-entity))
  (let* ((flying (fly-p entity))
         (yaw (neck-yaw entity))
         (dir (direction entity))
         (speed (movement-speed entity))
         (yvalue  (if flying
                      (cond
                        ((jump-p entity) speed)
                        ((sneak-p entity) (- speed))
                        (t 0.0))
                      (vec-y (velocity entity)))))
    (if dir
        (let ((diraux (+ dir yaw)))
          (set-temp-vec
           (* speed (- (sin diraux)))
           yvalue
           (* speed (cos diraux))))
        (set-temp-vec 0.0 yvalue 0.0))))

(defun apply-movement-force (entity dt)
  (let* ((current-velocity (velocity entity))
         (target-velocity (target-velocity entity))
         (step-power (step-power entity))
         (delta-velocity (vec-
                          target-velocity
                          current-velocity))
         (dv-magnitude (vec-magnitude delta-velocity)))
    (unless (zerop dv-magnitude)
      (let ((bump-direction (vec/ 
                             delta-velocity
                             dv-magnitude)))
        (let ((step-force (* 2.0 dv-magnitude)))
          (apply-force entity
                       (vec* bump-direction
                             (* step-power step-force))
                       dt))))))

(defgeneric step-position (entity delta)
  (:documentation "Increment ENTITY's position by DELTA, taking
  collision in to account"))

(defmethod step-position (entity delta)
  (vec-incf (pos entity) delta))

(defmethod step-position ((entity has-world-collision) delta)
  "Increment ENTITY's position by delta, clamping position and
velocity to prevent clipping with the world"
  (if (not (clip-p entity))
      (vec-incf (pos entity) delta)
      (with-vec (px py pz) ((pos entity))
        (with-vec (dx dy dz) (delta)
          (let ((dead-axis #b000)
                (clamp #b000)
                (aabb (aabb entity)))
            (flet ((deadify (x bit)
                     (when (zerop x)
                       (logiorf dead-axis bit))))
              (deadify dx #b100)
              (deadify dy #b010)
              (deadify dz #b001))
            (step-recursive entity px py pz dx dy dz clamp dead-axis aabb))))))

(defun step-recursive (entity px py pz dx dy dz clamp dead-axis aabb)
  (if (= #b111 dead-axis)
      (progn
        (vec-setf (velocity entity) 0.0 0.0 0.0)
        (vec-setf (pos entity) px py pz))
      (multiple-value-bind (newclamp ratio)
          (collect-touch px py pz dx dy dz aabb)
        (let ((whats-left (- 1 ratio)))
          (macrolet ((axis (pos delta bit)
                       `(unless (logtest ,bit dead-axis)
                          (incf ,pos (* ratio ,delta))
                          (if (logtest ,bit newclamp)
                              (setf ,delta 0.0)
                              (*= ,delta whats-left)))))
            (axis px dx #b100)
            (axis py dy #b010)
            (axis pz dz #b001))
          (if (>= 0 whats-left)
              (progn (vec-setf (pos entity) px py pz)
                     (when (logtest #b100 clamp)
                       (setf (vec-x (velocity entity)) 0.0))
                     (when (logtest #b010 clamp)
                       (setf (vec-y (velocity entity)) 0.0))
                     (when (logtest #b001 clamp)
                       (setf (vec-z (velocity entity)) 0.0)))
              (step-recursive entity
                              px py pz
                              dx dy dz
                              (logior clamp newclamp)
                              (logior dead-axis newclamp)
                              aabb))))))

(defun collect-touch (px py pz dx dy dz aabb)
  (aabbcc:with-touch-collector (collect-touch collapse-touch min-ratio)
    ;;[FIXME] aabb-collect-blocks does not check
    ;;slabs, only blocks upon entering.  also check
    ;;"intersecting shell blocks?"
    (aabbcc:aabb-collect-blocks (px py pz dx dy dz aabb)
        (x y z contact)
      (declare (ignorable contact))
      (let ((blockid (world:getblock x y z)))
        (when (block-data:data blockid :hard)
          (multiple-value-bind (minimum type)
              (aabbcc:aabb-collide aabb
                                   px py pz
                                   (block-to-block-aabb blockid)
                                   x y z
                                   dx dy dz)
            (collect-touch minimum type)))))
    (values (collapse-touch dx dy dz) min-ratio)))

(defgeneric step-velocity (entity delta)
  (:documentation "Increment ENTITY's velocity by DELTA, taking
  collision in to account"))

(defmethod step-velocity (entity delta)
  (vec-incf (velocity entity) delta))

(defmethod step-velocity :after ((entity has-world-collision) delta)
  "Nullify ENTITY's velocity where it is in contact with the world"
  (declare (ignore delta))
  (let ((contact-state (world-contact entity)))
    (flet ((nullify-velocity-where-obstructed (velocity i+ i- j+ j- k+ k-)
             ;;velocity is a 3 float array.
             ;;If we are moving along an axis, but the contact
             ;;state says that direction is obstructed, then
             ;;set the physical motion along that direction to 0
             (with-vec (xvel yvel zvel) (velocity symbol-macrolet)
               (when (or (and (plusp xvel) i+)
                         (and (minusp xvel) i-))
                 (setf xvel 0.0))
               (when (or (and (plusp yvel) j+)
                         (and (minusp yvel) j-))
                 (setf yvel 0.0))
               (when (or (and (plusp zvel) k+)
                         (and (minusp zvel) k-))
                 (setf zvel 0.0)))))
      (nullify-velocity-where-obstructed
       (velocity entity)
       (logtest contact-state #b100000)
       (logtest contact-state #b010000)
       (logtest contact-state #b001000)
       (logtest contact-state #b000100)
       (logtest contact-state #b000010)
       (logtest contact-state #b000001)))))

;;;;

(defparameter *particle-aabb* (create-aabb 0.1))
(defclass particle (entity)
  ((aabb :initform *particle-aabb*)
   ;;Lifetime in seconds
   (lifetime :initarg :lifetime
	     :initform 5.0)
   (alive-p :initform t
	    :accessor alive-p)
   (mass :initform 1.0)
   (uv :initform #(0.0 0.0 1.0 1.0)
       :initarg :uv
       :accessor particle-uv)))

(defparameter *particles* ())
(defun create-particle (blockid x y z &optional (dx 0.0) (dy 0.0) (dz 0.0) (lifetime 3.0))
  (floatf x y z dx dy dz)
  (multiple-value-bind (u0 v0 u1 v1)
      (block-data::fit-to-texture (block-data::texture-for-particle blockid)) 
    (let ((particle
	   (make-instance 'particle
			  :uv (vector u0 v0 u1 v1)
			  :pos (vec x y z)
			  :velocity (vec dx dy dz)
			  :lifetime lifetime)))
      (push particle *particles*)
      particle)))

(defmethod step-physics ((particle particle) dt)
  (with-slots (lifetime alive-p) particle
    (decf lifetime dt)
    (when (minusp lifetime)
      (setf alive-p nil))
    (when (on-ground particle)
      (let ((friction 0.93))
	(let ((vel (velocity particle)))
	  (vec* vel friction vel))))))

(defun run-particles (dt)
  (dolist (particle *particles*)
    (step-physics particle dt))
  (setf *particles* (remove-if-not 'alive-p *particles*)))
