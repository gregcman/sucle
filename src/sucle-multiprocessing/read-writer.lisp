(in-package :sump)

;; from https://gist.github.com/deadtrickster/5d8202a376309d1e822e
;; https://en.wikipedia.org/wiki/Readers%E2%80%93writers_problem
(export '(make-rwlock with-write-lock with-read-lock))
;; FIXME: rename from rwlock to r*w*lock

(defstruct rwlock
  (rlock (bt:make-lock))
  (wlock (bt:make-lock))
  (rtrylock (bt:make-lock))
  (resource (bt:make-lock))
  (rcount 0)
  (wcount 0))

(defun read-lock-begin (rwlock)
  (bt:acquire-lock (rwlock-rtrylock rwlock))
  (bt:acquire-lock (rwlock-rlock rwlock))
  (when (= 1 (incf (rwlock-rcount rwlock)))
    (bt:acquire-lock (rwlock-resource rwlock)))
  (bt:release-lock (rwlock-rlock rwlock))
  (bt:release-lock (rwlock-rtrylock rwlock)))

(defun read-lock-end (rwlock)
  (bt:acquire-lock (rwlock-rlock rwlock))
  (when (= 0 (decf (rwlock-rcount rwlock)))
    (bt:release-lock (rwlock-resource rwlock)))
  (bt:release-lock (rwlock-rlock rwlock)))

(defmacro with-read-lock (rwlock &body body)
  (alexandria:with-gensyms (rwlock%)
    `(let ((,rwlock% ,rwlock))
       (read-lock-begin ,rwlock%)
       (unwind-protect
            (progn ,@body)
         (read-lock-end ,rwlock%)))))

(defun write-lock-begin (rwlock)
  (bt:acquire-lock (rwlock-wlock rwlock))
  (when (= 1 (incf (rwlock-wcount rwlock)))
    (bt:acquire-lock (rwlock-rtrylock rwlock)))
  (bt:release-lock (rwlock-wlock rwlock))
  (bt:acquire-lock (rwlock-resource rwlock)))

(defun write-lock-end (rwlock)
  (bt:release-lock (rwlock-resource rwlock))
  (bt:acquire-lock (rwlock-wlock rwlock))
  (when (= 0 (decf (rwlock-wcount rwlock)))
    (bt:release-lock (rwlock-rtrylock rwlock)))
  (bt:release-lock (rwlock-wlock rwlock)))

(defmacro with-write-lock (rwlock &body body)
  (alexandria:with-gensyms (rwlock%)
    `(let ((,rwlock% ,rwlock))
       (write-lock-begin ,rwlock%)
       (unwind-protect
            (progn ,@body)
	 (write-lock-end ,rwlock%)))))
