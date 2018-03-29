(defun print-bits (n &optional (stream *standard-output*))
  (format stream "~64,'0b" n)
  n)

(defun spill-hash (hash)
  (loop for key being the hash-keys of hash
     using (hash-value value)
     do (format t "~S ~S~%" key value)))
