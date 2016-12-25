(in-package :macrology)

;;1. start with a bunch of constant specifications
;;2. generate multiple functions according to the specifications
;;3. specifications are in a hash table which hash names bound to constants
;;4. function generating functions are toplevel so they can be tested
(defun gen-spec ()
  (make-hash-table :test 'eq))

(defun add-spec (spec body)
  (dolist (pair body)
    (setf (gethash (car pair) spec) (cdr pair)))
  spec)

(defun spec-assoc (hash)
  (let ((acc nil))
    (maphash
     (lambda (k v)
       (push (cons k v) acc))
     hash)
    acc))

;;replace all symbols which start with p!x.. with
;;the value from the hash table with keyx..
(defun is-param (symbol)
  (let ((string (symbol-name symbol)))
    (if (> (length string) 2)
	(if (string= "P!" (subseq string 0 2))
	    (intern (subseq string 2) (symbol-package symbol))))))

;;rp = replace params
(defun rp (spec code)
  (labels ((rec (piece)
	     (if (atom piece)
		 (if (symbolp piece)
		     (let ((val (is-param piece)))
		       (if val
			   (gethash val spec)
			   piece))
		     piece)
		 (cons (rec (car piece))
		       (rec (cdr piece))))))
    (rec code)))

;;below is for dealing with function headers
;;is the symbol actually a parameter or a keyword

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun legal-p (symbol)
    (if (member symbol '(&optional &rest &body &key &optional &aux t))
	nil
	symbol))
  (defun get-actual-args (lambda-list)
    (let ((actual-args nil))
      (dolist (arg lambda-list actual-args)
	(if (atom arg)
	    (if (legal-p arg)
		(push arg actual-args))
	    (push (car arg) actual-args))))))

(defmacro defspec (name lambda-list)
  (let ((actual-args (get-actual-args lambda-list)))
    `(defun ,name ,(cons 'spec lambda-list)
       (add-spec spec ,(cons 'list (mapcar (lambda (x) (list 'cons (list 'quote x) x)) actual-args))))))
