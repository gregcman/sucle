(in-package :lem-sucle)

(lem:define-command forward2 (&optional (n 1)) ("p")
  (lem:forward-char 1)
  (lem-paredit-mode:paredit-forward n))
;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Interactive-Codes.html
;;This is what "p" and "r" are for?
(defun start-lem ()
  (let ((lem::*in-the-editor* nil))
    ;;(lem:main nil)
    ;;#+nil
    (lem:add-hook lem:*find-file-hook*
		  (lambda (buffer)
		    (when (eq (lem:buffer-major-mode buffer) 'lem-lisp-mode:lisp-mode)
		      (lem:change-buffer-mode buffer 'lem-paredit-mode:paredit-mode t))))
    (setf lem.term::*ansi-color-names-vector*
	  ;;from misterioso
	  (mapcar 'lem:parse-color
		  (remove-duplicates '("#2d3743" "#ff4242" "#74af68" "#dbdb95"
				       "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"
				       ;;above were from ansi-color-names-vector
				       ;;https://github.com/jwiegley/emacs-release/blob/master/etc/themes/misterioso-theme.el
				       "#878787" "#eeeeec" "#415160" "#2d4948"
				       "#212931" "#729fcf" "#23d7d7" "#ffad29"
				       "#e67128")
				     :test 'string=)))
    (lem.term::regen-color-array)
    (progn
      (define-sacred-keys)
      ;;(define-other-keys)
      (lem:define-key lem:*global-keymap* "C-/" 'lem:undo)
      (lem:define-key lem.language-mode:*language-mode-keymap* "Tab" 'indent-region-or-otherwise)
      (lem:define-key lem:*global-keymap* "Tab" 'indent-region-or-otherwise)
      (lem:define-key lem-paredit-mode:*paredit-mode-keymap* "C-k" 'lem:kill-sexp)
      (lem:define-key lem-lisp-mode:*lisp-mode-keymap* "C-k" 'lem:kill-sexp)
      (lem:define-key lem-paredit-mode:*paredit-mode-keymap* ")" 'forward2)
      (lem:define-key lem.listener-mode:*listener-mode-keymap* "C-Down"
	'lem.listener-mode:listener-next-input)
      (lem:define-key lem.listener-mode:*listener-mode-keymap* "C-Up"
	'lem.listener-mode:listener-prev-input)
      (lem:define-key lem:*global-keymap* "Return"
	'lem.language-mode:newline-and-indent)
      (progn
	;;FIXME::where to put this?
	(lem::clear-all-attribute-cache)
	#+nil
	(ncurses-clone::reset-ncurses-color-pairs)
	#+nil
	(lem.term::reset-color-pair))
      (lem:lem)
      (lem:send-event
       (lambda ()
	 (lem:find-file (merge-pathnames "other/example.lisp"
					 (asdf:system-source-directory :lem-opengl)))
	 (lem-paredit-mode:paredit-mode)
	 (lem:load-theme "misterioso"))))
    (lem-sucle::input-loop)))

(in-package :lem-user)

(define-color-theme "misterioso" ()
  ;;(display-background-mode :dark)
  (foreground "#e1e1e0") ;;
  (background ;;"#3a3a3a" ;;
	      "#2d3743"
	      )
  (cursor :background "#415160"
	  ;;FIXME::what is the correct foreground? ;;not perfect, not same as modeline background,
	  ;;but good enough?
	  :foreground "#212931") ;;
  (region :background "#2d4948" :foreground "#e1e1e0") ;;
  (modeline :background "#212931" :foreground "#eeeeec") ;;
  (modeline-inactive :background "#878787" :foreground "#eeeeec");;
  (minibuffer-prompt-attribute :foreground "#729fcf" :bold-p t) ;;
  (syntax-builtin-attribute :foreground "#23d7d7") ;;
  (syntax-comment-attribute :foreground "#74af68");;
  (syntax-constant-attribute :foreground "#008b8b");;
  (syntax-function-name-attribute :foreground "#00ede1" :bold-p t);;
  (syntax-keyword-attribute :foreground "#ffad29" :bold-p t);;
  (syntax-string-attribute :foreground "#e67128");;
  (syntax-type-attribute :foreground "#34cae2") ;;
  (syntax-variable-attribute :foreground "#dbdb95") ;;
  (syntax-warning-attribute :foreground "#dbdb95" :bold-p t))
(in-package :lem-sucle)

;;https://en.wikipedia.org/wiki/Keyboard_shortcut
;;FIXME::cross-platform unified interface, or per-OS interface?
;;C-X, C-G, M-X
(defun define-sacred-keys ()
  (lem:define-key lem:*global-keymap* "Delete" 'delete-region-or-char)
  (lem:define-key lem:*global-keymap* "Backspace" 'delete-region-or-char)
  (lem:define-key lem:*global-keymap* "C-z" 'lem:undo)
  #+nil
  (progn
    (lem:define-key lem:*global-keymap* "C-a" 'lem::mark-set-whole-buffer)
    (lem:define-key lem:*global-keymap* "C-s" 'lem:save-buffer)
    (lem:define-key lem:*global-keymap* "C-f" 'lem.isearch:isearch-forward)
    ;;FIXME::C-G?
    (lem:define-key lem:*global-keymap* "C-v" 'lem:paste-from-clipboard)
    (lem:define-key lem:*global-keymap* "C-x" 'delete-region)
    (lem:define-key lem:*global-keymap* "C-c" 'lem:copy-region))

  ;;(lem:define-key lem:*global-keymap* "C-c" 'lem:yank)
  )

(defun define-other-keys ()
  (lem:define-key lem:*global-keymap* "C-?" 'lem:describe-key))

(lem:define-command delete-region-or-char () ()
  (let ((buffer (lem:current-buffer)))
    (if (lem:buffer-mark-p buffer)
	(%delete-region buffer)
	(;;lem:delete-character ;;FIXME::dispatch on mode?
	 lem-paredit-mode:paredit-backward-delete))))

(lem:define-command indent-region-or-otherwise () ()
  (let ((buffer (lem:current-buffer)))
    (if (lem:buffer-mark-p buffer)
	(progn
	  (print 3434)
	  (lem:indent-region (lem:buffer-mark buffer)
			     (lem:buffer-point buffer)))
	(lem.language-mode::indent-line-and-complete-symbol))))

(lem:define-command delete-region () ()
  (let ((buffer (lem:current-buffer)))
    (when (lem:buffer-mark-p buffer)
      (%delete-region buffer))))


(defun %delete-region (buffer)
  (lem:delete-between-points
   (lem:buffer-mark buffer)
   (lem:buffer-point buffer)))

#+nil ;;FIXME::remove this unused code?
(progn
  (defparameter *packages* nil)
  (defun find-lem-package ()
    (remove-if-not (lambda (x)
		     (prefix-p "LEM" 
			       (package-name x)))
		   (list-all-packages)))
;;;FIXME::see cepl.examples/cleanup for similar code
  #+nil
  (setf *packages* (find-lem-package))
  (defun find-variables (&optional (packages *packages*))
    (let ((acc nil))
      (dolist (package packages)
	(do-symbols (sym package)
	  (when (boundp sym)
	    (when (eq (symbol-package sym)
		      package)
	      (push sym acc)))))
      acc))
  (defun prefix-p (prefix string)
    "test whether prefix is a prefix of string"
    (let ((len (length prefix)))
      (search prefix string
	      :start1 0 :end1 len
	      :start2 0 :end2 (min len (length string))))))
