(in-package :sucle)

;;;;************************************************************************;;;;
;;emacs-like modes
(defparameter *active-modes* ())
(defun reset-all-modes ()
  (setf *active-modes* nil))
(defun enable-mode (mode)
  (pushnew mode *active-modes* :test 'equal))
(defun disable-mode (mode)
  (setf *active-modes* (delete mode *active-modes*)))
(defun mode-enabled-p (mode)
  (member mode *active-modes* :test 'equal))
(defun set-mode-if (mode p)
  (if p
      (enable-mode mode)
      (disable-mode mode)))
