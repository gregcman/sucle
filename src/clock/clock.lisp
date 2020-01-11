(defpackage :clock
  (:use #:cl)
  (:export #:microseconds))
(in-package :clock)
(deftype seconds ()
  ;;FIXME::arbitrary amount
  '(unsigned-byte 40))
(deftype nanosecond ()
  `(integer 0 ,(1- (expt 10 9))))
(defun microseconds ()
  (declare (optimize (speed 3) (safety 0)))
  #+(or allegro cmu sbcl abcl ccl (and lispworks (or linux darwin)))
  (let ((zeroed-seconds (load-time-value (local-time:timestamp-to-unix (local-time:now)))))
    (declare (type seconds zeroed-seconds))
    (multiple-value-bind (sec nsec) (local-time::%get-current-time)
      (declare (type nanosecond nsec)
	       (type seconds sec))
      (+ (* 1000000 (- sec zeroed-seconds))
	 (floor nsec 1000))))
  #-(or allegro cmu sbcl abcl ccl (and lispworks (or linux darwin)))   
  (* (get-internal-real-time)
     (load-time-value
      (progn
	;;throw error if internal-time-units-per-second is less than 1000
	(when (> 1000 internal-time-units-per-second)
	  (error "no suitable clock found"))
	
	(round (/ 1000000 internal-time-units-per-second))))))

