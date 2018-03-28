(defun print-bits (n &optional (stream *standard-output*))
  (format stream "~64,'0b" n)
  n)
