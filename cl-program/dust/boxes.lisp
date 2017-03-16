(in-package :sandbox)

;;;various box sizes for different things

;;;its a cubic meter
(defun block-aabb ()
  (aabbcc::make-aabb
   :minx 0.0
   :miny 0.0
   :minz 0.0
   :maxx 1.0
   :maxy 1.0
   :maxz 1.0))

;;;a person's personal space
(defun player-aabb ()
  (aabbcc::make-aabb
   :minx -0.3
   :miny -1.5
   :minz -0.3
   :maxx 0.3
   :maxy 0.12
   :maxz 0.3))

(defun player-aabb+1 ()
  (aabbcc::make-aabb
   :minx -0.3
   :miny -0.5
   :minz -0.3
   :maxx 0.3
   :maxy 1.12
   :maxz 0.3))

(defun chunk-aabb ()
  (aabbcc::make-aabb
   :minx -8.0
   :miny -8.0
   :minz -8.0
   :maxx 8.0
   :maxy 8.0
   :maxz 8.0))
(defparameter chunk-aabb (chunk-aabb))
(defparameter player-aabb+1 (player-aabb+1))

;;;a very small cubic fist
(defun fist-aabb ()
  (aabbcc::make-aabb
   :minx -0.005
   :miny -0.005
   :minz -0.005
   :maxx 0.005
   :maxy 0.005
   :maxz 0.005))

(defparameter block-aabb (block-aabb))
(defparameter player-aabb (player-aabb))
(defparameter fist-aabb (fist-aabb))
