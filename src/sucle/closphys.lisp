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
  (:export))

(in-package #:physics)


;;;; Utilities

;; Access nsb-cga:vec through wrapper macros in case the
;; implementation of vectors changes
(deftype vec () 'nsb-cga:vec) 

(defparameter *temp-vec* (nsb-cga:vec 0.0 0.0 0.0))

(defmacro modify (fun a &rest rest)
  (once-only (a)
    `(,fun ,a ,a ,@rest)))

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

(defmacro vec+ (a f &optional (temp-vec '*temp-vec*))
  `(nsb-cga:%vec+ ,temp-vec ,a ,f))

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
  ((velocity :type vec
             :initarg :velocity
             :initform (vec 0.0 0.0 0.0)
             :accessor velocity)
   (acceleration :type vec
                 :initform (vec 0.0 0.0 0.0)
                 :accessor acceleration))
  (:documentation "An object that moves through the world"))

(defclass has-drag ()
  ((drag-coefficient :type float
                     :initform 0.0003
                     :initarg :drag-coefficient
                     :accessor drag-coefficient))
  (:documentation "An object which experiences air friction"))

(defclass has-mass ()
  ((mass :type float
         :initarg :mass
         :accessor mass))
  (:documentation "An object with a mass"))

(defmethod apply-force ((object has-mass) force)
  (vec-incf (acceleration object)
            (vec* force (mass object))))

(defmethod apply-impulse ((object has-mass) impulse dt)
  (vec-incf (velocity object)
            (vec* (vec* impulse (/ 1 (mass object))) dt)))

(defvar *default-acceleration-due-to-gravity* (vec 0.0 -13.0 0.0))
(defclass has-gravity ()
  ((gravity-p :type boolean
              :initform t
              :accessor gravity-p))
  (:documentation "An object whose motion is affected by gravity."))

(defmethod acceleration-due-to-gravity ((object has-gravity))
  *default-acceleration-due-to-gravity*)

;; method on the acceleration accessor function. Doesn't implement the
;; frame of gravity lag, but I'll figure that out later.
(defmethod acceleration :around ((object has-gravity))
  "Apply acceleration due to gravity before returning acceleration."
  (if (gravity-p object)
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

(defun on-ground (entity)
  (when (typep entity 'has-world-collision)
    (logtest (world-contact entity) 4)))

;; (defclass point-projectile (has-physics has-drag) ()
;;   (:documentation "An object "))

(defclass entity (has-physics
                  has-drag
                  has-mass
                  has-world-collision)
  ())

(defclass living-entity (entity)
  ((neck-yaw :type float
             :initform 0.0
             :accessor neck-yaw)
   (neck-pitch :type float
               :initform 0.0
               :accessor neck-pitch)
   (hips :type (or nil float)
         :initform nil
         :accessor hips)))

(defclass player-entity (living-entity)
  ((fly-p :type boolean
          :initform nil
          :accessor fly-p)
   (sneak-p :type boolean
            :initform nil
            :accessor sneak-p)))

;;;; Physics system
(defgeneric step-physics (entity dt)
  (:documentation "Entry point to the physics system. Modify ENTITY to
  have experienced DT seconds"))

(defmethod step-physics (entity dt))

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
  "Initialize acceleration to zero, run all physics methods, then step
velocity and positon"
  (vec-setf (acceleration entity) 0 0 0)
  (call-next-method entity dt)
  (step-velocity entity (vec* (acceleration entity) dt))
  (step-position entity (vec* (velocity entity) dt)))

;; Maybe this needs changing
(defmethod step-physics :after ((entity has-drag) dt)
  "Apply the impulse of drag"
  (with-vec (vx vy vz) ((velocity entity))
    (let ((k (drag-coefficient entity)))
      (apply-impulse entity
                     (vec (* k (expt vx 2))
                          (* k (expt vy 2))
                          (* k (expt vz 2)))
                     dt))))

(defgeneric step-position (entity delta)
  (:documentation "Increment ENTITY's position by DELTA, taking
  collision in to account."))

(defmethod step-position (entity delta)
  (vec-incf (pos entity) delta))

(defmethod step-position ((entity has-world-collision) delta)
  "Increment ENTITY's position by delta, clamping position and
velocity to prevent clipping with the world."
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
        (vec-setf (velocity entity) 0 0 0)
        (vec-setf (pos entity) px py pz))
      (multiple-value-bind (newclamp ratio)
          (collect-touch px py pz dx dy dz aabb)
        (let ((whats-left (- 1 ratio)))
          (macrolet ((axis (pos delta bit)
                       `(unless (logtest ,bit dead-axis)
                          (incf ,pos (* ratio ,delta))
                          (if (logtest ,bit newclamp)
                              (setf ,delta 0)
                              (*= ,delta whats-left)))))
            (axis px dx #b100)
            (axis py dy #b010)
            (axis pz dz #b001))
          (if (>= 0 whats-left)
              (progn (vec-setf (pos entity) px py pz)
                     (when (logtest #b100 clamp)
                       (setf (vec-x (velocity entity)) 0))
                     (when (logtest #b010 clamp)
                       (setf (vec-y (velocity entity)) 0))
                     (when (logtest #b001 clamp)
                       (setf (vec-z (velocity entity)) 0)))
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
  collision in to account."))

(defmethod step-velocity (entity delta)
  (vec-incf (velocity entity) delta))

(defmethod step-velocity :after ((entity has-world-collision) delta)
  "Nullify ENTITY's velocity where it is in contact with the world."
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
