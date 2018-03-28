(defstruct (zymbol (:constructor %make-zymbol (name hash)))
  (name (error "must provide name!") :type (or null (simple-array character (*))))
  (hash (error "NO") :type fixnum)
  (value nil :type t))

(defun make-zymbol (string)
  (declare (type (simple-array character *) string))
  (let ((hash-value (sxhash string)))
    (%make-zymbol string hash-value)))

(progn
  (defun pprint-zymbol (stream zymbol)
    (pprint-logical-block (stream nil)
      (format stream "#?~a" (zymbol-name zymbol))))
  (set-pprint-dispatch 'zymbol 'pprint-zymbol))

(progn
  (defun question-mark-reader (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (make-zymbol (string (read stream t nil t))))
  (set-dispatch-macro-character #\# #\? #'question-mark-reader))

(defparameter *all-zymbols* (make-hash-table :test 'equal))

(defun make-zymbol-hash ()
  (make-hash-table :test 'eq :hash-function (lambda (z) (zymbol-hash z))))
