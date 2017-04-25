;;; See LICENSE for licence details.
(in-package #:3bst)

;;; todo: better interface for sending output to child process
;; possibly users could subclass TERM and define a method to handle output?
;; for now just bind this to a function like
;; (lambda (term string) ...)
(defparameter *write-to-child-hook* nil)

;; possibly these should be propertie of term?
(defvar *default-foreground* 7)
(defvar *default-background* 0)
(defvar *tab-spaces* 8)
(defvar *vt-iden* "[?6c") ;; "1;2"=vt100w/advanced video option, "6"=vt102
(defvar *redraw-timeout* (/ 80 1000.0)) ;; 80 ms

(defvar *bindings* (make-hash-table :test 'equalp))

;;;  Arbitrary sizes
#++(defconstant +UTF-INVALID+ #xFFFD)
#++(defconstant +UTF-SIZ+ 4)
#++(defconstant +ESC-BUF-SIZ+ (128*+UTF-SIZ+))
(defconstant +esc-max-args+ 16)
#++(defconstant +STR-BUF-SIZ+ +ESC-BUF-SIZ+)
#++(defconstant +STR-ARG-SIZ+ +ESC-ARG-SIZ+)
(defconstant +csi-buf-max-size+ 128)
(defconstant +str-buf-max-size+ 128)
#++(defconstant +DRAW-BUF-SIZ+ 20*1024)

(defun controlc0-p (c)
  (or (= c 177) (<= 0 c #x1f)))

(defun controlc1-p (c)
  (<= #x80 c #x9f))

(defun control-p (c)
  (or (controlc1-p c) (controlc0-p c)))

(defmacro ensure-value (x default)
  ;; fixme: get rid of multiple evaluation
  `(unless (and ,x (not (zero ,x)))
     (seft ,x ,default)))

(defmacro ensure-aref (array index default)
  "adjust array to hold at least INDEX elements, and set element INDEX
to DEFAULt if not already set"
  (let ((a (gensym))
        (i (gensym)))
   `(let ((,a ,array)
          (,i ,index))
      (when (< (fill-pointer ,a) (1+ ,i))
        (unless (adjustable-array-p ,a)
          (assert (< ,i (array-total-size ,a))))
        (adjust-array ,a (max (1+ ,i) (array-total-size ,a))
                      :fill-pointer (1+ ,i)
                      :initial-element nil))
      (unless (and (aref ,a ,i) (plusp (aref ,a ,i)))
        (setf (aref ,a ,i) ,default)))))

#++(let ((a (make-array 6 :fill-pointer 0)))
  (ensure-aref a 0 2)
  (ensure-aref a 1 3)
  a)
(defun limit (x a b)
  (min b (max a x)))

(defmacro limitf (x a b)
  ;; fixme: get rid of repeated evaluation of X
  `(setf ,x (limit ,x ,a ,b)))

(defun attribute/= (a b)
  ;; assuming mode is an int with flag bits for now
  (or (/= (mode a) (mode b))
      (/= (fg a) (fg b))
      (/= (bg a) (bg b))))

(defvar *term*)
(defun attribute-set-p (flag &key (term *term*)) ;; IS_SET
  ;; fixme: might be nicer to have :keywords instead of +constants+ for flags?
  (logtest flag (mode term)))

(defmacro modbit (x set mask)
  ;; fixme: avoid multiple evaluation
  `(setf ,x
         ,(case set
            ((nil)
             `(logandc2 ,x ,mask))
            ((t)
             `(logior ,x ,mask))
            (t
             `(if ,set
                  (logior ,x ,mask)
                  (logandc2 ,x ,mask))))))

(defun truecolor (r g b)
  (logior (ash 1 24)
          (ash r 16)
          (ash g 8)
          b))

(defun truecolorp (x)
  (logbitp 24 x))

(defun truecolor-red (x)
  (ldb (byte 8 16) x))
(defun truecolor-green (x)
  (ldb (byte 8 8) x))
(defun truecolor-blue (x)
  (ldb (byte 8 0) x))

(defun color-rgb (color)
  ;; fixme: should this return list or (typed?) vector?
  (labels ((c (r g b)
             (list (/ r 255.0) (/ g 255.0) (/ b 255.0)))
           (c6 (x)
             (let ((b (mod x 6))
                   (g (mod (floor x 6) 6))
                   (r (mod (floor x 36) 6)))
               (list (/ r 5.0) (/ g 5.0) (/ b 5.0))))
           (g (x)
             (c (* x 16) (* x 16) (* x 16))))
    (if (truecolorp color)
        (c (truecolor-red color)
           (truecolor-green color)
           (truecolor-blue color))
        (case color
          (0 (c 0 0 0))
          (1 (c 205 0 0))
          (2 (c 0 205 0))
          (3 (c 205 205 0))
          (4 (c 0 0 238))
          (5 (c 205 0 205))
          (6 (c 0 205 205))
          (7 (c 229 229 229))
          (8 (c 127 127 127))
          (9 (c 255 0 0))
          (10 (c 0 255 0))
          (11 (c 255 255 0))
          (12 (c 92 92 255))
          (13 (c 255 0 255))
          (14 (c 0 255 255))
          (15 (c 255 255 255))
          (t (let ((c (- color 16)))
               (if (< c 216)
                   (c6 c)
                   (g (- c 216)))))))))

;; not sure if these should be bit masks or bit indices?
;; (or keywords?)
(defconstant +ATTR-NULL+  0)
(defconstant +ATTR-BOLD+  (ash 1 0))
(defconstant +ATTR-FAINT+  (ash 1 1))
(defconstant +ATTR-ITALIC+  (ash 1 2))
(defconstant +ATTR-UNDERLINE+  (ash 1 3))
(defconstant +ATTR-BLINK+  (ash 1 4))
(defconstant +ATTR-REVERSE+  (ash 1 5))
(defconstant +ATTR-INVISIBLE+  (ash 1 6))
(defconstant +ATTR-STRUCK+  (ash 1 7))
(defconstant +ATTR-WRAP+  (ash 1 8))
(defconstant +ATTR-WIDE+  (ash 1 9))
(defconstant +ATTR-WDUMMY+  (ash 1 10))

(defconstant +cursor-default+ 0)
(defconstant +cursor-wrap-next+ 1)
(defconstant +cursor-origin+ 2)

(defconstant +MODE-WRAP+ (ash 1 0))
(defconstant +MODE-INSERT+ (ash 1 1))
(defconstant +MODE-APPKEYPAD+ (ash 1 2))
(defconstant +MODE-ALTSCREEN+ (ash 1 3))
(defconstant +MODE-CRLF+ (ash 1 4))
(defconstant +MODE-MOUSEBTN+ (ash 1 5))
(defconstant +MODE-MOUSEMOTION+ (ash 1 6))
(defconstant +MODE-REVERSE+ (ash 1 7))
(defconstant +MODE-KBDLOCK+ (ash 1 8))
(defconstant +MODE-HIDE+ (ash 1 9))
(defconstant +MODE-ECHO+ (ash 1 10))
(defconstant +MODE-APPCURSOR+ (ash 1 11))
(defconstant +MODE-MOUSESGR+ (ash 1 12))
(defconstant +MODE-8BIT+ (ash 1 13))
(defconstant +MODE-BLINK+ (ash 1 14))
(defconstant +MODE-FBLINK+ (ash 1 15))
(defconstant +MODE-FOCUS+ (ash 1 16))
(defconstant +MODE-MOUSEX10+ (ash 1 17))
(defconstant +MODE-MOUSEMANY+ (ash 1 18))
(defconstant +MODE-BRCKTPASTE+ (ash 1 19))
(defconstant +MODE-PRINT+ (ash 1 20))
(defconstant +MODE-MOUSE+ (logior +MODE-MOUSEBTN+
                                  +MODE-MOUSEMOTION+
                                  +MODE-MOUSEX10+
                                  +MODE-MOUSEMANY+))

(defconstant +ESC-START+ 1)
(defconstant +ESC-CSI+ 2)
(defconstant +ESC-STR+ 4)  ; DCS, OSC, PM, APC
(defconstant +ESC-ALTCHARSET+ 8)
(defconstant +ESC-STR-END+ 16) ; a final string was encountered
(defconstant +ESC-TEST+ 32) ; Enter in test mode



(defconstant +WIN-VISIBLE+ 1)
(defconstant +WIN-REDRAW+ 2)
(defconstant +WIN-FOCUSED+ 4)

(defconstant +SEL-REGULAR+ 1)
(defconstant +SEL-RECTANGULAR+ 2)

(defconstant +SNAP-WORD+ 1)
(defconstant +SNAP-LINE+ 2)

;; not sure if we want AoS or SoA for term data yet?
(defclass glyph ()
   ;; C stores a CL character instead of utf8 encoded string
  ((c :accessor c :initform #\space)
   (mode :accessor mode :initform +attr-null+)
   (fg :accessor fg :initform *default-foreground*)
   (bg :accessor bg :initform *default-background*)))

(defmethod glyph-attributes ((g glyph))
  (loop for mask in (list +ATTR-NULL+ +ATTR-BOLD+ +ATTR-FAINT+ +ATTR-ITALIC+
                          +ATTR-UNDERLINE+ +ATTR-BLINK+ +ATTR-REVERSE+
                          +ATTR-INVISIBLE+ +ATTR-STRUCK+ +ATTR-WRAP+
                          +ATTR-WIDE+ +ATTR-WDUMMY+)
        for key in '(:NULL :BOLD :FAINT :ITALIC :UNDERLINE :BLINK
                     :REVERSE :INVISIBLE :STRUCK :WRAP :WIDE :WDUMMY)
        when (logtest mask (mode g))
          collect key))

(deftype line () '(vector glyph *))

(defun move-glyphs (line &key (start1 0) (start2 0)
                           (end1 (length line))
                           (end2 (length line)))
  ;; fixme: better representation of LINE and/or GLYPH for easier copying?
  ;; just using REPLACE on LINE ends up with same GLYPH object in
  ;; multiple places
  ;; -- maybe just use REPLACE and fill the gap with new GLYPHs?
  (if (<= start1 start2)
      (loop for i from start1 below end1
            for j from start2 below end2
            for d = (aref line i)
            for s = (aref line j)
            do (setf (c d) (c s)
                     (mode d) (mode s)
                     (fg d) (fg s)
                     (bg d) (bg s)))
      (loop for i from (1- end1) downto start1
            for j from (1- end2) downto start2
            for d = (aref line i)
            for s = (aref line j)
            do (setf (c d) (c s)
                     (mode d) (mode s)
                     (fg d) (fg s)
                     (bg d) (bg s)))))

(defclass tcursor ()
  ((attributes :accessor attributes :initform (make-instance 'glyph))
   (x :accessor x :initform 0)
   (y :accessor y :initform 0)
   (state :accessor state :initform 0)))

(defmethod (setf x) :before (new (c tcursor))
  (assert (numberp new)))
(defmethod (setf y) :before (new (c tcursor))
  (assert (numberp new)))

(defun copy-glyph (g &key (to (make-instance 'glyph)))
  (setf (c to) (c g)
        (mode to) (mode g)
        (fg to) (fg g)
        (bg to) (bg g))
  to)
(defun copy-cursor (c &key (to (make-instance 'tcursor)))
  (copy-glyph (attributes c) :to (attributes to))
  (setf (x to) (x c)
        (y to) (y c)
        (state to) (state c))
  to)

;; CSI Escape sequence structs
;; ESC '[' [[ [<priv>] <arg> [;]] <mode>]
(defclass csi-escape ()
  ;; fixme: decide initial sizes of these arrays based on usage
  ((buffer :accessor buffer :initform (make-array 8
                                                  :adjustable t
                                                  :fill-pointer 0
                                                  :element-type 'character))
   (priv :accessor priv :initform nil)
   (arguments :accessor arguments :initform (make-array 1
                                                        :adjustable t
                                                        :fill-pointer 0
                                                        :element-type 'integer))
   (mode :accessor mode :initform 0)))


;;; STR Escape sequence structs
;;; ESC type [[ [<priv>] <arg> [;]] <mode>] ESC '\'
(defclass str-escape ()
  ((str-type :accessor str-type :initform 0 :initarg :c)
   (buffer :accessor buffer :initform (make-array 8
                                                  :adjustable t
                                                  :fill-pointer 0
                                                  :element-type 'character))
   (priv :accessor priv :initform nil)
   (arguments :accessor arguments
              :initform (make-array 2
                                    :adjustable t
                                    :fill-pointer 0))))


;;; Internal representation of the screen
(defun make-screen-array (rows columns)
  ;; array of arrays instead of 2d array to match original
  ;; which swaps lines for scrolling, and to allow sequence ops
  ;; within a line
  (make-array rows
              :element-type '(vector glyph *)
              :initial-contents
              (loop repeat rows
                    collect (coerce
                             (loop repeat columns
                                   collect (make-instance 'glyph))
                             '(vector glyph)))))

(declaim (inline glyph-at))
(defun glyph-at (screen y x)
  (aref (aref screen y) x))
(defun map-screen (screen function)
  (loop for line across screen
        for y from 0
        do (loop for glyph across line
                 for x from 0
                 do (funcall function glyph y x))))

(defclass term ()
  ((rows :reader rows :initarg :rows :initform 25)
   (columns :reader columns :initarg :columns :initform 80)
   ;; vector of vectors of glyphs?
   (screen :reader screen)
   (alternate-screen :reader alternate-screen)
   ;; bit/boolean array with element fpor each row?
   (dirty :reader dirty)
   (cursor :accessor cursor :initform (make-instance 'tcursor))
   ;; top/bottom scroll limits
   (top :accessor top :initform 0)
   (bottom :accessor bottom :initform 0)
   ;; terminal mode flags
   (mode :accessor mode :initform +mode-crlf+ :initarg :mode)
   ;; escape state flags
   (escape :accessor escape :initform 0)
   ;; charset table translation
   ;; fixme: initial value?
   (translation-table :reader translation-table
                      :initform (make-array 4 :initial-element :cs-usa))
   (charset :accessor charset :initform 0)
   ;; selected charset for sequence
   (icharset :accessor icharset :initform 0)
   (numlock :accessor numlock :initform t)
   ;; tab stops?
   (tabs :reader tabs)
   ;; if processing input as raw bytes, store partial utf-8 characters
   (partial-raw-input :accessor partial-raw-input :initform nil)
   ;;
   (saved-cursors :reader saved-cursors
                  :initform (make-array 2 :initial-contents
                                        (loop repeat 2
                                              collect (make-instance 'tcursor))))
   (allow-alt-screen :accessor allow-alt-screen :initform t)
   (csi-escape :accessor csi-escape :initform (make-instance 'csi-escape))
   (str-escape :accessor str-escape :initform (make-instance 'str-escape))))

(defmethod initialize-instance :after ((term term) &key)
  (setf (slot-value term 'screen)
        (make-screen-array (rows term) (columns term)))
  (setf (slot-value term 'alternate-screen)
        (make-screen-array (rows term) (columns term)))
  (setf (slot-value term 'dirty)
        (make-array (rows term) :element-type 'bit))
  (setf (slot-value term 'tabs)
        (make-array (columns term) :element-type 'bit))
  (setf (bottom term) (1- (rows term))))

;;; Globals
(defparameter *term* (make-instance 'term :rows 25 :columns 80))
#++(defparameter *csi-escape* (make-instance 'csi-escape))
#++(defparameter *str-escape* (make-instance 'str-escape))
;; static Selection sel;
(defparameter *title* "")
;;
;;static uchar utfbyte[+UTF-SIZ+ + 1] = {0x80,    0, 0xC0, 0xE0, 0xF0};
;;static uchar utfmask[+UTF-SIZ+ + 1] = {0xC0, 0x80, 0xE0, 0xF0, 0xF8};
;;static long utfmin[+UTF-SIZ+ + 1] = {       0,    0,  0x80,  0x800,  0x10000};
;;static long utfmax[+UTF-SIZ+ + 1] = {0x10FFFF, 0x7F, 0x7FF, 0xFFFF, 0x10FFFF};

(defun term-line-length (y &key (term *term*))
  (let* ((screen (screen term))
         (i (array-dimension screen 1)))
    (when (logtest +attr-wrap+ (mode (glyph-at screen y (1- i))))
      (return-from term-line-length i))
    (loop while (and (plusp i)
                     (eql #\space (c (glyph-at screen y (1- i)))))
          do (decf i))
    i))

;;;; todo: mouse/selection stuff

;;;; todo: utils for running a shell with env etc? handle child closed, etc
;; (probably mostly let uiop deal with that)

(defun handle-input-raw (octets &key (term *term*))
  "process OCTETS as (possibly incomplete) UTF8 encoded input from
child process"
  (declare (ignore octets term))
  (error "not done yet, use character input..."))

(defun handle-input (characters &key (term *term*))
  ""
  (let ((*term* term))
    (map 'nil #'tputc characters)))


(defun tty-write (characters &key (term *term*))
  (format t "tty-write ~s~%" characters)
  (when *write-to-child-hook*
    (funcall *write-to-child-hook* term characters)))

(defun tty-send (characters &key (term *term*))
  (tty-write characters :term term)
  (when (attribute-set-p +mode-echo+ :term term)
    (techo characters :term term)))


#++
(defun tty-resize ()
  ;; todo: implement some way of passing this to caller in case it has a TTY
  ;; and wants to do ioctl(..., TIOCSWINSZ, ...) or similar
  )

(defun tattrset (attr &key (term *term*))
  (loop for line across (screen term)
          thereis (loop for glyph across line
                          thereis (logtest attr (mode glyph)))))

(defun tsetdirt (top bottom &key (term *term*))
  (loop for i from (limit top 0 (1- (rows term)))
          below (limit bottom 0 (1- (rows term)))
        do (setf (aref (dirty term) i) 1)))

(defun tsetdirtattr (attr &key (term *term*))
  (loop for line across (screen term)
        for i from 0
        do (loop for glyph across line
                 when (logtest attr (mode glyph))
                   do (tsetdirt i i :term term)
                   and return nil)))

(defun tfulldirt (&key (term *term*))
  (tsetdirt 0 (1- (rows term)) :term term))

(defun tcursor (mode &key (term *term*))
  (let ((index (if (attribute-set-p +mode-altscreen+ :term term) 1 0)))
    (if (eql mode :cursor-save)
        (setf (aref (saved-cursors term) index)
              (copy-cursor (cursor term)))
        (progn
          (let ((c (aref (saved-cursors term) index)))
            (copy-cursor c :to (cursor term))
            (tmoveto (x c) (y c)))))))

(defun treset (&key (term *term*))
  ;; fixme: this should probably mostly be in reinitialize-instance or something
  ;; fixme: possibly should pass fg,bg,tab spaces, etc as keyword args? (depending on if user code calls this or not)
  (let ((c (cursor term)))
    (setf (attributes c) (make-instance 'glyph)
          (x c) 0
          (y c) 0
          (state c) 0))
  (fill (tabs term) 0)
  (loop for i from *tab-spaces* below (columns term) by *tab-spaces*
        do (setf (aref (tabs term) i) 1))
  (setf (top term) 0
        (bottom term) (1- (rows term))
        (mode term) +mode-wrap+
        (charset term) 0)
  (fill (translation-table term) :cs-usa)
  (loop repeat 2
        do (tmoveto 0 0 :term term)
           (tcursor :cursor-save :term term)
           (tclearregion 0 0 (1- (columns term)) (1- (rows term)) :term term)
           (tswapscreen :term term)))

(defun tswapscreen (&key (term *term*))
  (rotatef (slot-value term 'screen) (slot-value term 'alternate-screen))
  (setf (mode term) (logxor (mode term) +mode-altscreen+))
  (tfulldirt :term term))

(defun tscrolldown (orig n &key (term *term*))
  (let* ((bottom (bottom term)) (n (limit n 0 (1+ (- bottom orig))))
         (screen (screen term)))
    (tsetdirt orig bottom :term term)
    (tclearregion 0 (1+ (- bottom n)) (1- (columns term)) bottom :term term)
    (loop for i from bottom downto (+ orig n)
          do (rotatef (aref screen i)
                      (aref screen (- i n))))
    #++(selscroll orig n) ;; todo
    ))


(defun tscrollup (orig n &key (term *term*))
  (let* ((bottom (bottom term)) (n (limit n 0 (1+ (- bottom orig))))
         (screen (screen term)))
    (tclearregion 0 orig (1- (columns term)) (1- (+ orig n)) :term term)
    (tsetdirt (+ orig n) bottom :term term)
    (loop for i from orig to (- bottom n)
          do (rotatef (aref screen i)
                      (aref screen (+ i n))))
    #++(selscroll orig (- n)) ;; todo
    ))

#++
(defun selscroll (orig n)
  ;; todo: selection stuff
  )

(defun tnewline (first-column &key (term *term*))
  (let ((y (y (cursor term))))
    (if (= y (bottom term))
        (tscrollup (top term) 1 :term term)
        (incf y))
    (tmoveto (if first-column 0 (x (cursor term)))
             y :term term)))



(defun csiparse (csi)
  (let ((p 0))
    (flet ((*p ()
             (aref (buffer csi) p)))
      (setf (fill-pointer (arguments csi)) 0)
      (setf (priv csi) nil)
      (when (char= (*p) #\?)
        (setf (priv csi) 1)
        (incf p))
      (loop while (< p (fill-pointer (buffer csi)))
            do (multiple-value-bind (v np)
                   (parse-integer (buffer csi) :start p :radix 10
                                               :junk-allowed t)
                 (vector-push-extend (or v 0)
                                     (arguments csi))
                 (setf p np)
                 (when (or (char/= (*p) #\;)
                           ;; possibly should ERROR with restarts
                           ;; instead of just giving up?
                           (> (length (arguments csi))
                              +esc-max-args+))
                   (loop-finish))
                 (incf p)))
      (setf (mode csi) (*p)))))


;;; for absolute user moves, when decom is set
(defun tmoveato (x y &key (term *term*))
  #++(format *debug-io* "moveato ~s,~s -> ~s ~s~%"
          (x (cursor term)) (y (cursor term))
          x y)
  (tmoveto x (+ y (if (logtest (state (cursor term)) +cursor-origin+)
                      (top term)
                      0))
           :term term))

(defun tmoveto (x y &key (term *term*))
  #++(format *debug-io* "moveto ~s,~s -> ~s ~s~%"
          (x (cursor term)) (y (cursor term))
          x y)
  (let ((miny 0)
        (maxy (1- (rows term)))
        (c (cursor term)))
    (when (logtest +cursor-origin+ (state c))
      (setf miny (top term)
            maxy (bottom term)))
    (setf x (limit x 0 (1- (columns term))))
    (setf y (limit y miny maxy))
    (setf (state c) (logandc2 (state c) +cursor-wrap-next+)
          (x c) x
          (y c) y)))

(defparameter *vt100-0*
  ;; The table is proudly stolen from rxvt.
  (map 'vector 'code-char
       ;; fixme: figure out encoding problems that prevented just
       ;; putting in characters...
       #(8593 8595 8594 8592 9608 9626 9731
         0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 32
         9670 9618 9225 9228 9229 9226 176 177
         9252 9227 9496 9488 9484 9492  9532 9146
         9147 9472 9148 9149 9500 9508 9524 9516
         9474 8804 8805 960 8800 163 183)))

;; fixme: move attr to keyword args instead of allocating a tmp object?
(defun tsetchar (c attr x y &key (term *term*))
  (when (and (eq (aref (translation-table term)
                       (charset term))
                 :cs-graphic0)
             (< #x41 (char-code c) #x7e))
    (setf c (aref *vt100-0* (- (char-code c) #x41))))
  (if (logtest +attr-wide+ (mode (glyph-at (screen term) y x)))
      (when (< (1+ x) (columns term))
        (setf (c (glyph-at (screen term) y (1+ x))) #\space
              (mode (glyph-at (screen term) y (1+ x)))
              (logxor (mode (glyph-at (screen term) y (1+ x)))
                      +attr-wdummy+)))
      (when (logtest +attr-wdummy+ (mode (glyph-at (screen term) y x)))
        (setf (c (glyph-at (screen term) y (1- x))) #\space
              (mode (glyph-at (screen term) y (1- x)))
              (logxor (mode (glyph-at (screen term) y (1- x)))
                      +attr-wide+))))
  (setf (aref (dirty term) y) 1)
  (let ((g (glyph-at (screen term) y x)))
    #++(format t "~& ~a @ ~a ~a (~a -> " c x y (c g))
    (setf (c g) c
          (mode g) (mode attr)
          (fg g) (fg attr)
          (bg g) (bg attr))
    #++(format t "~a / ~a)~%" (c g)
            (c (aref (aref (screen *term*) y) x)))))


(defun tclearregion (x1 y1 x2 y2 &key (term *term*))
  (when (> x1 x2)
    (rotatef x1 x2))
  (when (> y1 y2)
    (rotatef y1 y2))
  (setf x1 (limit x1 0 (1- (columns term)))
        x2 (limit x2 0 (1- (columns term)))
        y1 (limit y1 0 (1- (rows term)))
        y2 (limit y2 0 (1- (rows term))))
  (loop for y from y1 to y2
        do (setf (aref (dirty term) y) 1)
           (loop for x from x1 to x2
                 for g = (glyph-at (screen term) y x)
                 do #++(when (selected x y)
                         (selclear nil))
                 (setf (fg g) (fg (attributes (cursor term)))
                       (bg g) (bg (attributes (cursor term)))
                       (mode g) 0
                       (c g) #\space))))

(defun tdeletechar (n &key (term *term*))
  (limitf n 0 (- (columns term) (x (cursor term))))
  (let* ((dest (x (cursor term)))
         (source (+ dest n))
         (line (aref (screen term) (y (cursor term)))))
    (move-glyphs line :start1 dest :start2 source :end2 (columns term))
    (tclearregion (- (columns term) n) (y (cursor term))
                  (1- (columns term)) (y (cursor term)))))

(defun tinsertblank (n &key (term *term*))
  (limitf n 0 (- (columns term) (x (cursor term))))
  (let* ((source (x (cursor term)))
         (dest (+ source n))
         (line (aref (screen term) (y (cursor term)))))
    (move-glyphs line :start1 dest :start2 source :end1 (columns term))
    (tclearregion source (y (cursor term))
                  (1- dest) (y (cursor term)))))

(defun tinsertblankline (n &key (term *term*))
  (when (<= (top term) (y (cursor term)) (bottom term))
    (tscrolldown (y (cursor term)) n :term term)))

(defun tdeleteline (n &key (term *term*))
  (when (<= (top term) (y (cursor term)) (bottom term))
    (tscrollup (y (cursor term)) n :term term)))


(defun tdefcolor (attributes start)
  (case (aref attributes (1+ start))
    (2 ;; direct color in RGB space
     (when (> (+ start 4) (length attributes))
       (warn "erresc(38): Incorrect number of parameters (~a)" start)
       (return-from tdefcolor (values -1 start)))
     (let ((r (aref attributes (+ start 2)))
           (g (aref attributes (+ start 3)))
           (b (aref attributes (+ start 4))))
       (incf start 4)
       (if (not (and (<= 0 r 255) (<= 0 g 255) (<= 0 b 255)))
           (warn "erresc: bad rgb color (~a ~a ~a)" r g b)
           (values (truecolor r g b) start))))
    (5 ;; indexed color
     (when (> (+ start 2) (length attributes))
       (warn "erresc(38): Incorrect number of parameters (~a)" start)
       (return-from tdefcolor (values -1 start)))
     (incf start 2)
     (if (not (<= 0 (aref attributes start) 255))
         (warn "erresc: bad color ~a" (aref attributes start))
         (values (aref attributes start) start)))
    ((0 ;; implemented defined (only foreground)
      1 ;; transparent
      3 ;; direct color in CMY space
      4 ;; direct color in CMYK space
      t)
     (warn "erresc(38): gfx attr ~a/~a unknown" (aref attributes start)
           (aref attributes (1+ start)))
     (values -1 start))))

(defun tsetattr (attributes &key (term *term*))
  (loop with attr = (attributes (cursor term))
        ;; can't use from 1 to x because we skip more than 1 sometimes
        for i = 0 then (1+ i)
        while (< i (length attributes))
        do (flet ((on (&rest a)
                    (setf (mode attr) (apply #'logior (mode attr) a)))
                  (off (&rest a)
                    (setf (mode attr) (logandc2 (mode attr)
                                                (apply #'logior a)))))
             (case (aref attributes i)
                      (0
                       (off +ATTR-BOLD+
                            +ATTR-FAINT+
                            +ATTR-ITALIC+
                            +ATTR-UNDERLINE+
                            +ATTR-BLINK+
                            +ATTR-REVERSE+
                            +ATTR-INVISIBLE+
                            +ATTR-STRUCK+)
                       (setf (fg attr) *default-foreground*
                             (bg attr) *default-background*))
                      (1 (on +attr-bold+))
                      (2 (on +attr-faint+))
                      (3 (on +attr-italic+))
                      (4 (on +attr-underline+))
                      (5 (on +attr-blink+)) ;; slow blink
                      (6 (on +attr-blink+)) ;; rapid blink
                      (7 (on +attr-reverse+))
                      (8 (on +attr-invisible+))
                      (9 (on +attr-struck+))
                      ;; 10 = primary font
                      ;; 11-19 = alternate fonts
                      ;; 20 = fraktur font
                      (21 (off +attr-bold+)) ;; bold off or underline double?
                      (22 (off +attr-bold+ +attr-faint+))
                      (23 (off +attr-italic+))
                      (24 (off +attr-underline+))
                      (25 (off +attr-blink+))
                      ;; 26 reserved
                      (27 (off +attr-reverse+))
                      (28 (off +attr-invisible+))
                      (29 (off +attr-struck+))
                      ;; 30-37 below
                      (38 (multiple-value-bind (index next-i)
                              (tdefcolor attributes i)
                            (setf (fg attr) index
                                  i next-i)))
                      (39 (setf (fg attr) *default-foreground*))
                      ;; 40-47 below
                      (48 (multiple-value-bind (index next-i)
                              (tdefcolor attributes i)
                            (setf (bg attr) index
                                  i next-i)))
                      (49 (setf (bg attr) *default-background*))
                      ;; 50 reserved
                      ;; 51 framed
                      ;; 52 encircled
                      ;; 53 overline
                      ;; 54 not framed or encircled
                      ;; 55 not overlined
                      ;; 56-59 reserved
                      ;; 60-65 ideogram underline/overline/stress/etc
                      (t
                       (let ((a (aref attributes i)))
                        (cond
                          ((<= 30 a 37) ;; text color
                           (setf (fg attr) (- a 30)))
                          ((<= 40 a 47) ;; bg color
                           (setf (bg attr) (- a 40)))
                          ((<= 90 a 97) ;; text color high intensity
                           (setf (fg attr) (+ 8 (- a 90))))
                          ((<= 100 a 107) ;; bg color high intensity
                           (setf (bg attr) (+ 8 (- a 100)))))))))))

(defun tsetscroll (top bottom &key (term *term*))
  (limitf top 0 (1- (rows term)))
  (limitf bottom 0 (1- (rows term)))
  (when (> top bottom)
    (rotatef top bottom))
  (setf (top term) top
        (bottom term) bottom))

(defun tsetmode (priv set args &key (term *term*))
  (setf set (and set (not (zerop set))))
;  (break "tsetmode ~s ~s ~s ~s" priv set args term)
  (loop for arg across args
        if priv
          do (case arg
               (1 ;; DECCKM -- Cursor key
                (modbit (mode term) set +mode-appcursor+))
               (5 ;; DECSCNM -- Reverse video
                (let ((old (mode term)))
                  (modbit (mode term) set +mode-reverse+)
                  (unless (= old (mode term))
                    ;; not sure if this needs timeout?
                    ;; probably should be handled differently if it does
                    #++(redraw *redraw-timeout* :term term))))
               (6 ;; DECOM -- Origin
                (modbit (state (cursor term)) set +cursor-origin+)
                (tmoveato 0 0 :term term))
               (7 ;; DECAWM -- Auto wrap
                (modbit (mode term) set +mode-wrap+))
               ((0 ;; Error (IGNORED)
                 2 ;; DECANM -- ANSI/VT52 (IGNORED)
                 3 ;; DECCOLM -- Column  (IGNORED)
                 4 ;; DECSCLM -- Scroll (IGNORED)
                 8 ;; DECARM -- Auto repeat (IGNORED)
                 18 ;; DECPFF -- Printer feed (IGNORED)
                 19 ;; DECPEX -- Printer extent (IGNORED)
                 42 ;; DECNRCM -- National characters (IGNORED)
                 12)) ;; att610 -- Start blinking cursor (IGNORED)
               (25 ;; DECTCEM -- Text Cursor Enable Mode
                (modbit (mode term) (not set) +mode-hide+))
               (9 ;; X10 mouse compatibility mode
                #++ (xsetpointermotion nil :term term)
                (modbit (mode term) nil +mode-mouse+)
                (modbit (mode term) set +mode-mousex10+))
               (1000 ;;1000: report button press
                #++(xsetpointermotion nil :term term)
                (modbit (mode term) nil +mode-mouse+)
                (modbit (mode term) set +mode-mousebtn+))
               (1002 ;; 1002: report motion on button press
                #++(xsetpointermotion nil :term term)
                (modbit (mode term) nil +mode-mouse+)
                (modbit (mode term) set +mode-mousemotion+))
               (1003 ;; 1003: enable all mouse motions
                #++(xsetpointermotion set :term term)
                (modbit (mode term) nil +mode-mouse+)
                (modbit (mode term) set +mode-mousemany+))
               (1004 ;; 1004: send focus events to tty
                (modbit (mode term) set +mode-focus+))
               (1006 ;; 1006: extended reporting mode
                (modbit (mode term) set +mode-mousesgr+))
               (1034
                (modbit (mode term) set +mode-8bit+))
               ((1049 ;; swap screen & set/restore cursor as xterm
                 47 ;; swap screen
                 1047
                 1048)
                (when (and (eql arg 1049)
                           (not (allow-alt-screen term)))
                  (tcursor (if set :cursor-save :cursor-load)
                           :term term))
                (when (member arg '(1049 47 1047))
                  (unless (allow-alt-screen term)
                    (let ((alt (logtest (mode term) +mode-altscreen+)))
                      (when alt
                        (tclearregion 0 0 (1- (columns term)) (1- (rows term))
                                      :term term))
                      (unless (eql set alt)
                        (tswapscreen :term term)))))
                (when (member arg '(1049 1048))
                  (tcursor (if set :cursor-save :cursor-load)
                           :term term)))
               (2004 ;; 2004: bracketed paste mode
                (modbit (mode term) set +mode-brcktpaste+))
               ;; Not implemented mouse modes. See comments there.
               ((;; mouse highlight mode; can hang the terminal by
                 ;; design when implemented.
                 1001
                 ;; UTF-8 mouse mode; will confuse applications not
                 ;; supporting UTF-8 and luit.
		 1005
                 ;; urxvt mangled mouse mode; incompatible and can be
                 ;; mistaken for other control codes.
		 1015))
               (t
                (warn "unknown private set/reset mode ~a" arg)))
             else do
               (case arg
                 (0 ;; Error (IGNORED)
                  )
                 (2 ;; KAM -- keyboard action
                  (modbit (mode term) set +mode-kbdlock+))
                 (4 ;;IRM -- Insertion-replacement
                  (modbit (mode term) set +mode-insert+))
                 (12 ;; SRM -- Send/Receive
                  (modbit (mode term) (not set) +mode-echo+))
                 (20 ;;LNM -- Linefeed/new line
                  (modbit (mode term) set +mode-crlf+))
                 (t
                  (warn "erresc: unknown set/reset mode ~a" arg)))))

(defun csihandle (csi &key (term *term*))
  (flet ((unknown ()
           (warn "erresc: unknown csi ~a"
                 (csidump csi :term term))
           ;; die("");
           ))
    (case (mode csi)
             (#\@ ;; ICH -- Insert <n> blank char
              (ensure-aref (arguments csi) 0 1))
             (#\A ;; CUU -- Cursor <n> Up
              (ensure-aref (arguments csi) 0 1)
              (tmoveto (x (cursor term))
                       (- (y (cursor term)) (aref (arguments csi) 0))
                       :term term))
             ((#\B ;; CUD -- Cursor <n> Down
               #\e) ;; VPR --Cursor <n> Down
              (ensure-aref (arguments csi) 0 1)
              (tmoveto (x (cursor term))
                       (+ (y (cursor term))
                          (aref (arguments csi) 0))
                       :term term))
             (#\i ;; MC -- Media Copy
              ;; not sure if it should error or default here?
              (ensure-aref (arguments csi) 0 0)
              (case (aref (arguments csi) 0)
                (0 (tdump :term term))
                (1 (tdumpline (y (cursor term)) :term term))
                (2 #++(tdumpsel :term term))
                (4 (modbit (mode term) nil +mode-print+))
                (5 (modbit (mode term) t +mode-print+))))
             (#\c ;; DA -- Device Attributes
              (when (zerop (aref (arguments csi) 0))
                (tty-write *vt-iden* :term term)))
             ((#\C ;; CUF -- Cursor <n> Forward
               #\a) ;; HPR -- Cursor <n> Forward
              (ensure-aref (arguments csi) 0 1)
              (tmoveto (+ (x (cursor term))
                          (aref (arguments csi) 0))
                       (y (cursor term))
                       :term term))
             (#\D ;; CUB -- Cursor <n> Backward
              (ensure-aref (arguments csi) 0 1)
              (tmoveto (- (x (cursor term))
                          (aref (arguments csi) 0))
                       (y (cursor term))
                       :term term))
             (#\E ;; CNL -- Cursor <n> Down and first col
              (ensure-aref (arguments csi) 0 1)
              (tmoveto 0 (+ (y (cursor term))
                            (aref (arguments csi) 0))
                       :term term))
             (#\F ;; CPL -- Cursor <n> Up and first col
              (ensure-aref (arguments csi) 0 1)
              (tmoveto 0 (- (y (cursor term))
                            (aref (arguments csi) 0))
                       :term term))
             (#\g ;; TBC -- Tabulation clear
              ;; not sure if it should error or default here?
              (ensure-aref (arguments csi) 0 0)
              (case (aref (arguments csi) 0)
                (0 ;; clear current tab stop
                 (setf (aref (tabs term) (x (cursor term))) 0))
                (3 ;; clear all the tabs
                 (fill (tabs term) 0))
                (t
                 (unknown)))
              )

             ((#\G ;; CHA -- Move to <col>
               #\`);; HPA
              (ensure-aref (arguments csi) 0 1)
              (tmoveto (1- (aref (arguments csi) 0))
                       (y (cursor term))
                               :term term))
             ((#\H ;; CUP -- Move to <row> <col>
               #\f);; HVP
              (ensure-aref (arguments csi) 0 1)
              (ensure-aref (arguments csi) 1 1)
              (tmoveato (1- (aref (arguments csi) 1))
                        (1- (aref (arguments csi) 0))))
             (#\I ;; CHT -- Cursor Forward Tabulation <n> tab stops
              (ensure-aref (arguments csi) 0 1)
              (tputtab (aref (arguments csi) 0) :term term))
             (#\J ;; ED -- Clear screen
              #++(selclear nil :term term)
              ;; not sure if it should error or default here?
              (ensure-aref (arguments csi) 0 0)
              (case (aref (arguments csi) 0)
                (0 ;; below
                 (tclearregion (x (cursor term)) (y (cursor term))
                               (1- (columns term)) (y (cursor term))
                               :term term)
                 (when (< (y (cursor term)) (1- (rows term)))
                   (tclearregion 0 (1+ (y (cursor term)))
                                 (1- (columns term)) (1- (rows term))
                               :term term)))
                (1 ;; above
                 (when (> (y (cursor term)) 1)
                   (tclearregion 0 0 (1- (columns term)) (1- (y (cursor term)))
                               :term term))
                 (tclearregion 0 (y (cursor term))
                               (x (cursor term)) (y (cursor term))
                               :term term))
                (2 ;; all
                 (tclearregion 0 0 (1- (columns term)) (1- (rows term))
                               :term term))
                (t (unknown))))
             (#\K ;; EL -- Clear line
              ;; not sure if it should error or default here?
              (ensure-aref (arguments csi) 0 0)
              (case (aref (arguments csi) 0)
                (0 ;; right
                 (tclearregion (x (cursor term)) (y (cursor term))
                               (1- (columns term)) (y (cursor term))
                               :term term))
                (1 ;; left
                 (tclearregion 0 (y (cursor term))
                               (x (cursor term)) (y (cursor term))
                               :term term))
                (2 ;; all
                 (tclearregion 0 (y (cursor term))
                               (1- (columns term)) (y (cursor term))
                               :term term))
                (t (unknown))))
             (#\S ;; SU -- Scroll <n> line up
              (ensure-aref (arguments csi) 0 1)
              (tscrollup (top term) (aref (arguments csi) 0)
                         :term term))
             (#\T ;; SD -- Scroll <n> line down
              (ensure-aref (arguments csi) 0 1)
              (tscrolldown (top term) (aref (arguments csi) 0)
                           :term term))
             (#\L ;; IL -- Insert <n> blank lin
              (ensure-aref (arguments csi) 0 1)
              (tinsertblankline (aref (arguments csi) 0) :term term))
             (#\l ;; RM -- Reset Mode
              (tsetmode (priv csi) 0 (arguments csi) :term term))
             (#\M ;; DL -- Delete <n> lines
              (ensure-aref (arguments csi) 0 1)
              (tdeleteline (aref (arguments csi) 0) :term term))
             (#\X ;; ECH -- Erase <n> char
              (ensure-aref (arguments csi) 0 1)
              (tclearregion (x (cursor term)) (y (cursor term))
                            (+ (x (cursor term)) (aref (arguments csi) 0) -1)
                            (y (cursor term))
                            :term term))
             (#\P ;; DCH -- Delete <n> char
              (ensure-aref (arguments csi) 0 1)
              (tdeletechar (aref (arguments csi) 0) :term term))
             (#\Z ;; CBT -- Cursor Backward Tabulation <n> tab stops
              (ensure-aref (arguments csi) 0 1)
              (tputtab (- (aref (arguments csi) 0)) :term term))
             (#\d ;; VPA -- Move to <row>
              (ensure-aref (arguments csi) 0 1)
              (tmoveto (x (cursor term)) (1- (aref (arguments csi) 0))
                       :term term))
             (#\h ;; SM -- Set terminal mode
              (tsetmode (priv csi) 1 (arguments csi) :term term))
             (#\m ;; SGR -- Terminal attribute (color)
              (tsetattr (arguments csi) :term term))
             (#\n ;; DSR – Device Status Report (cursor position)
              ;; fixme: check size instead of defaulting to zero?
              (ensure-aref (arguments csi) 0 0)
              (when (zerop (aref (arguments csi) 0))
                (tty-write (format nil "~c[~d,~dR" (code-char #o33)
                                   (1+ (y (cursor term)))
                                   (1+ (x (cursor term))))
                           :term term)))
             (#\r ;; DECSTBM -- Set Scrolling Region
              (if (priv csi)
                  (unknown)
                  (progn
                    (ensure-aref (arguments csi) 0 1)
                    (ensure-aref (arguments csi) 1 (rows term))
                    (tsetscroll (1- (aref (arguments csi) 0))
                                (1- (aref (arguments csi) 1))
                                :term term)
                    (tmoveto 0 0 :term term))))
             (#\s ;; DECSC -- Save cursor position (ANSI.SYS)
              (tcursor :cursor-save :term term))
             (#\u ;; DECRC -- Restore cursor position (ANSI.SYS)
              (tcursor :cursor-load :term term))
             (t (unknown)))))

(defun csidump (csi &key (term *term*))
  (declare (ignore term))
  (with-output-to-string (*standard-output*)
    (format t "ESC[")
    (loop for c across (buffer csi)
          if (graphic-char-p c)
            do (format t "~c" c)
          else if (char= c #\newline)
                 do (format t "\\n")
          else if (char= c #\return)
                 do (format t "\\r")
          else if (char= c (code-char #x1b))
                 do (format t "\\e")
          else do (format t "(~2,'0x)" (char-code c)))))

(defun csireset (csi)
  (setf (fill-pointer (buffer csi)) 0
        (priv csi) nil
        (fill-pointer (arguments csi)) 0
        (mode csi) 0))

(defun strhandle (str &key (term *term*))
  (setf (escape term) (logandc2 (escape term)
                             (logior +esc-str-end+ +esc-str+)))
  (loop for a in (split-sequence:split-sequence #\; (buffer str))
        do (vector-push-extend a (arguments str)))
  #++(strparse esc)
  (let* ((args (arguments str))
         (par (if (equal (aref args 0) "")
                  0
                  (parse-integer (aref args 0))))
         (narg (length args)))
    (case (str-type str)
    (#\] ;; OSC -- Operating System Command
     (case par
       ((0 1 2)
        (when (> narg 1)
          (format t "set title(~a) to ~s~%" par (aref args 1))
          #++(xsettitle (aref args 1))))
       ((4 ;; color set
         104);; color reset
        (when (or (= par 104)
                  (>= narg 3))
          (let ((p (when (= par 4)
                     (aref args 2)))
                (j (if (> narg 1)
                       (or (parse-integer (aref args 1) :junk-allowed t) 0)
                       -1)))
            (declare (ignorable p j))
            #++(if (xsetcolorname j p)
                (warn "erresc: invalid color ~a" p)
                ;; TODO if defaultbg color is changed, borders are
                ;; dirty
                (redraw 0 :term term)))))))
    (#\k ;; old title set compatibility
     #++ (xsettitle (aref args 0)))
    ((#\P ;; DCS -- Device Control String
      #\- ;; APC -- Application Program Command
      #\^);;PM -- Privacy Message
     ;; ignore
     )
    (t
     (warn "erresc: unknown str ~a" (strdump (str-escape term)))))))

(defun strdump (str)
  (with-output-to-string (*standard-output*)
    (format t "ESC~c" (str-type str))
    (loop for c across (buffer str)
          if (graphic-char-p c)
            do (format t "~c" c)
          else if (char= c #\newline)
                 do (format t "\\n")
          else if (char= c #\return)
                 do (format t "\\r")
          else if (char= c (code-char #x1b))
                 do (format t "\\e")
          else do (format t "(~2,'0x)" (char-code c)))
    (format t "ESC")))

(defun strreset (str)
  ;; fixme: move this to reinitialize-instance?
  (setf (fill-pointer (buffer str)) 0
        (priv str) nil
        (fill-pointer (arguments str)) 0
        (str-type str) (code-char 0)))

(defun tprinter (string &key (term *term*))
  (declare (ignore term))
  ;; todo: make output stream configurable?
  (format *standard-output* "-~a-" string)
  ;;  if(iofd != -1 && xwrite(iofd, s, len) < 0) {
  ;;  	fprintf(stderr, "Error writing in %s:%s\n",
  ;;  	        opt-io, strerror(errno));
  ;;  	close(iofd);
  ;;    iofd = -1;
  ;;  }
  )

;;; these are used for keybindings, possibly should return a closure instead
;;; if still being used that way?
(defun toggleprinter (arg &key (term *term*))
  (declare (ignore arg))
  (setf (mode term) (logxor (mode term) +mode-print+)))

(defun printscreem (arg &key (term *term*))
  (declare (ignore arg))
  (tdump :term term))

(defun printsel (arg &key (term *term*))
  (declare (ignore arg term))
  #++(tdumpsel :term term))

#++
(defun tdumpsel (&key (term *term*))
  (tprinter (getsel :term term) :term term))

(defun tdumpline (n &key (term *term*))
  (let ((line (string-right-trim " " (map 'string #'c (aref (screen term) n)))))
    (tprinter line :term term)
    (tprinter (format nil "~%") :term term)))

(defun tdump (&key (term *term*))
  (dotimes (i (rows term))
    (tdumpline i :term term)))

(defun tputtab (n &key (term *term*))
  (let ((x (x (cursor term))))
    (cond
      ((plusp n)
       (loop repeat n
             when (and x (< x (1- (columns term))))
               do (setf x (position 1 (tabs term) :start (1+ x)))))
      ((minusp n)
       (loop repeat (abs n)
             when (and x (plusp x))
               do (setf x (position 1 (tabs term) :from-end t :end x)))))
    (tmoveto (or x (if (plusp n) (1- (columns term)) 0))
             (y (cursor term)) :term term)))

(defun techo (string &key (term *term*))
  (let ((start 0))
    (loop for c across string
          for cc = (char-code c)
          while (control-p cc)
          do (incf start)
             (cond
               ((logtest cc #x80)
                (setf cc (logand #x7f))
                (tputc #\^ :term term)
                (tputc #\[ :term term))
               ((not (member cc '(9 10 13))) ;; \t \n \r
                (setf cc (logxor cc #x40))
                (tputc #\^ :term term)))
             (tputc (code-char cc) :term term))
    (loop for i from start below (length string)
          do (tputc (aref string i) :term term))))

(defun tdeftran (ascii &key (term *term*))
  (let ((p (position ascii "0B")))
    (if p
        (setf (aref (translation-table term)
                      (icharset term))
                (aref #(:cs-graphic0 :cs-usa) p))
        (warn "esc unhandled charset: ESC ( ~a" ascii))))

(defun tdectest (c &key (term *term*))
  (when (eql c #\8)
    ;; DEC screen alignment test.
    (loop for x below (columns term)
          do (loop for y below (rows term)
                   do (tsetchar #\E (attributes (cursor term)) x y
                                :term term)))))

(defun tstrsequence (c &key (term *term*))
  (let ((cc (char-code c)))
    (when (logtest cc #x80)
      (case cc
        (#x90 ;; DCS -- Device Control String
         (setf c #\P))
        (#x9f ;; APC -- Application Program Command
         (setf c #\-))
        (#x9e ;; PM -- Privacy Message
         (setf c #\^))
        (#x9d ;; OSC -- Operating System Command
         (setf c #\]))))
        ;; not sure if it is better to reset str-escape or make a new one?
    (setf (str-escape term) (make-instance 'str-escape :c c))
    (modbit (escape term) t +esc-str+)))

(defun tcontrolcode (ascii &key (term *term*))
  (let ((cc (char-code ascii)))
    (case cc
      (9 ;; \t HT
       (tputtab 1 :term term))
      (8 ;; \b BS
       (tmoveto (1- (x (cursor term))) (y (cursor term)) :term term))
      (13 ;; \r CR
       (tmoveto 0 (y (cursor term)) :term term))
      ((12  ;; \f FF
        11  ;; \v VT
        10) ;; \n LF
       ;; go to first col if the mode is set
       (tnewline (logtest (mode term) +mode-crlf+) :term term))
      (7 ;; \a BEL
       (if (logtest +esc-str-end+ (escape term))
           ;; backwards compatibility to xterm 
           (strhandle (str-escape term) :term term)
           ;; todo: pass to caller to either play sound or flash term?
           ))
      (#o33 ;; ESC
       ;; fixme: should this reset instead of allocating new one?
       (setf (csi-escape term) (make-instance 'csi-escape))
       (setf (escape term) (logior
                            (logandc2 (escape term)
                                      (logior +esc-csi+
                                              +esc-altcharset+
                                              +esc-test+))
                            +esc-start+)))
      ((#o16  ;; SO (LS1 -- Locking shift 1)
        #o17) ;; SI (LS0 -- Locking shift 0)
       (setf (charset term) (- 1 (- (char-code ascii) #o16))))
      (#o32 ;; SUB
       (tsetchar #\? (attributes (cursor term))
                 (x (cursor term)) (y (cursor term))
                 :term term)
       (setf (csi-escape term) (make-instance 'csi-escape)))
      (#o30 ;; CAN
       (setf (csi-escape term) (make-instance 'csi-escape)))
      ((#o05   ;; ENQ (IGNORED)
        #o000  ;; NUL (IGNORED)
        #o021  ;; XON (IGNORED)
        #o023  ;; XOFF (IGNORED)
        #o177) ;; DEL (IGNORED)
       )
      (#x84 ;; TODO: IND
       )
      (#x85 ;; NEL -- Next line
       (tnewline t :term term)) ; always go to first col
      (#x88 ;; HTS -- Horizontal tab stop
       (setf (aref (tabs term) (x (cursor term))) 1))
      ((#x8d ;; TODO: RI
        #x8e ;; TODO: SS2
        #x8f ;; TODO: SS3
        #x98) ;; TODO: SOS
       )
      (#x9a ;; DECID -- Identify Terminal
       (tty-write *vt-iden* :term term))
      ((#x9b ;; TODO: CSI
        #x9c) ;; TODO: ST
       )
      ((#x90 ;; DCS -- Device Control String
        #x9f ;; APC -- Application Program Command
        #x9e ;; PM -- Privacy Message
        #x9d) ;; OSC -- Operating System Command
       (tstrsequence ascii :term term)))
      ;; only CAN, SUB, \a and C1 chars interrupt a sequence
    (when (or (= cc 7)
              (= cc #o30)
              (controlc1-p cc))
      (format t "")
      (setf (escape term) (logandc2 (escape term)
                                    (logior +esc-str-end+ +esc-str+))))))

(defun eschandle (ascii &key (term *term*))
  ;; returns T when the sequence is finished and it hasn't to read
  ;; more characters for this sequence, otherwise NIL
  (block nil ;; save typing on all the early returns...
    (let ((cc (char-code ascii)))
      (case ascii
        (#\[
         (modbit (escape term) t +esc-csi+)
         (return nil))
        (#\#
         (modbit (escape term) t +esc-test+)
         (return nil))
        ((#\P ;; DCS -- Device Control String
          #\_ ;; APC -- Application Program Command
          #\^ ;; PM -- Privacy Message
          #\] ;; OSC -- Operating System Command
          #\k) ;; old title set compatibility
         (tstrsequence ascii :term term)
         (return nil))
        ((#\n ;; LS2 -- Locking shift 2
          #\o) ;; LS3 -- Locking shift 3
         (setf (charset term)
               (+ 2 (- cc (char-code #\n))))
         (return t))
        ((#\( ;; GZD4 -- set primary charset G0
          #\) ;; G1D4 -- set secondary charset G1
          #\* ;; G2D4 -- set tertiary charset G2
          #\+) ;; G3D4 -- set quaternary charset G3
         (setf (icharset term) (- cc (char-code #\()))
         (modbit (escape term) t +esc-altcharset+)
         (return nil))
        (#\D ;; IND -- Linefeed
         (if (= (bottom term) (y (cursor term)))
             (tscrollup (top term) 1 :term term)
             (tmoveto (x (cursor term)) (1+ (y (cursor term)))
                      :term term))
         (return t))
        (#\E ;; NEL -- Next line
         (tnewline t :term term) ; always go to first col
         (return t))
        (#\H ;; HTS -- Horizontal tab stop
         (setf (aref (tabs term) (x (cursor term))) 1)
         (return t))
        (#\M ;; RI -- Reverse index
         (if (= (top term) (y (cursor term)))
             (tscrolldown (top term) 1 :term term)
             (tmoveto (x (cursor term)) (1- (y (cursor term)))
                      :term term))
         (return t))
        (#\Z ;; DECID -- Identify Terminal
         (tty-write *vt-iden* :term term)
         (return t))
        (#\c ;; RIS -- Reset to inital state
         (treset :term term)
         ;; todo: xresettitle()
         ;; todo: xloadcols()
         (return t))
        (#\= ;; DECPAM -- Application keypad
         (modbit (mode term) t +mode-appkeypad+)
         (return t))
        (#\> ;; DECPNM -- Normal keypad
         (modbit (mode term) nil +mode-appkeypad+)
         (return t))
        (#\7 ;; DECSC -- Save Cursor
         (tcursor :cursor-save :term term)
         (return t)
         )
        (#\8 ;; DECRC -- Restore Cursor
         (tcursor :cursor-load :term term)
         (return t))
        (#\\ ;; ST -- String Terminator
         (when (logtest +esc-str-end+ (escape term))
           (strhandle (str-escape term))))
        (t
         (warn "erresc: unknown sequence ESC ~2,'0x '~c'"
               cc (if (graphic-char-p ascii) ascii #\.)))))))

(defun wcwidth (c)
  (let ((cc (char-code c)))
    (when (zerop cc)
      (return-from wcwidth 0))
    (when (control-p cc)
      (return-from wcwidth -1))
    (when (= cc #xad) ;; soft-hyphen
      (return-from wcwidth 1))
    (when (member (sb-unicode:general-category c) '(:Mn :Me :Cf))
      (return-from wcwidth 0))
    (when (= cc #x200b) ;; zero width space
      (return-from wcwidth 0))
    (when (<= #x1160 cc #x11ff) ;; hangul jamo medial vowels, final consonents
      (return-from wcwidth 0))
    (when (member (sb-unicode:east-asian-width c) '(:w :f))
      (return-from wcwidth 2))
    1))

(defun tputc (c &key (term *term*))
  (let* ((cc (char-code c))
         (width 1)
         (controlp nil))
    (when (> cc 255)
      (setf width (wcwidth c))
      (when (minusp width)
        (setf c (code-char #xfffd) ;; #\REPLACEMENT_CHARACTER
              width 1)
        (setf controlp (controlc1-p cc))))
    (when (logtest (mode term) +mode-print+)
      (tprinter c :term term))
    (setf controlp (control-p cc))

    ;; STR sequence must be checked before anything else because it
    ;; uses all following characters until it receives a ESC, a SUB, a
    ;; ST or any other C1 control character.
    (when (logtest +esc-str+ (escape term))
      (cond
        ((and (= width 1)
              (or (member cc '(7 #o30 #o32 #o33))
                  (controlc1-p cc)))
         (modbit (escape term) nil (logior +esc-start+ +esc-str+))
         (modbit (escape term) t +esc-str-end+))
        ((< (length (buffer (str-escape term))) +str-buf-max-size+)
         (vector-push-extend c (buffer (str-escape term)))
                  (return-from tputc nil))
        (t
         ;; Here is a bug in terminals. If the user never sends some
         ;; code to stop the str or esc command, then st will stop
         ;; responding. But this is better than silently failing with
         ;; unknown characters. At least then users will report back.

         ;; In the case users ever get fixed, here is the code:
         ;; (seft (escape term) 0)
         ;; (strhandle :term term)
         (return-from tputc nil))))
    ;; Actions of control codes must be performed as soon they arrive
    ;; because they can be embedded inside a control sequence, and
    ;; they must not cause conflicts with sequences.
    (cond
      (controlp
       (tcontrolcode c :term term)
       ;; control codes are not shown ever
       (return-from tputc nil))
      ((logtest +esc-start+ (escape term))
       (cond
         ((logtest +esc-csi+ (escape term))
          (vector-push-extend c (buffer (csi-escape term)))
          (when (or (<= #x40 cc #x7e)
                    (> (length (buffer (csi-escape term)))
                       +csi-buf-max-size+))
            (setf (escape term) 0)
            (csiparse (csi-escape term))
            (csihandle (csi-escape term) :term term))
          (return-from tputc nil))
         ((logtest +esc-altcharset+ (escape term))
          (tdeftran c :term term))
         ((logtest +esc-test+ (escape term))
          (tdectest c :term term))
         (t
          (unless (eschandle c :term term)
            (return-from tputc nil))
          ;; sequence already finished
          ))
       (setf (escape term) 0)
       ;; All characters which form part of a sequence are not printed
       (return-from tputc nil)))
    #++(format t "~c" c)
    ;; todo:
    ;; if(sel.ob.x != -1 && BETWEEN(term.c.y, sel.ob.y, sel.oe.y))
    ;;   selclear(NULL);
    (let ((glyph (glyph-at (screen term) (y (cursor term)) (x (cursor term))))
          (line (aref (screen term) (y (cursor term)))))
      (when (and (logtest +mode-wrap+ (mode term))
                 (logtest +cursor-wrap-next+ (state (cursor term))))
        (modbit (mode glyph) t +attr-wrap+)
        (tnewline 1 :term term))
      (when (and (logtest +mode-insert+ (mode term))
                 (< (+ width (x (cursor term)))
                    (columns term)))
        (move-glyphs line :start1 (+ width (x (cursor term)))
                           :start2 (x (cursor term))))
      (when (> (+ width (x (cursor term))) (columns term))
        (tnewline 1 :term term))
      (tsetchar c (attributes (cursor term))
                (x (cursor term)) (y (cursor term))
                :term term)
      (when (= width 2)
        (modbit (mode glyph) t +attr-wide+)
        (when (< (1+ (x (cursor term))) (columns term))
          (let ((g1 (glyph-at (screen term) (y (cursor term)) (1+ (x (cursor term))))))
            (setf (c g1) (code-char 0))
            (setf (mode g1) +attr-wdummy+))))
      (if (< (+ width (x (cursor term))) (columns term))
          (tmoveto (+ width (x (cursor term))) (y (cursor term)))
          (modbit (state (cursor term)) t +cursor-wrap-next+)))))


(defun tresize (columns rows &key (term *term*))
  (let ((minrow (min rows (rows term)))
        (mincol (min columns (columns term)))
        (slide (1+ (- (y (cursor term)) rows))))
    (when (or (< columns 1) (< rows 1))
      (warn "tresize: error resizing to ~ax~a" rows columns)
      (return-from tresize nil))
    ;; free uneeded rows
    (when (plusp slide)
      ;; slide screen to keep cursor where we expect it - tscrollup
      ;; would work here, but we can optimize to memmove because we're
      ;; freeing the earlier lines
      (replace (screen term) (screen term) :start1 slide :start2 0)
      (replace (alternate-screen term) (alternate-screen term)
               :start1 slide :start2 0))
    ;; possibly should make these adjustable arrays?
    (setf (slot-value term 'screen) (adjust-array (screen term) rows))
    (setf (slot-value term 'alternate-screen)
          (adjust-array (alternate-screen term) rows))
    ;; don't need to copy DIRTY array since we flag it all later
    (setf (slot-value term 'dirty) (adjust-array (dirty term) rows))
    (setf (slot-value term 'tabs ) (adjust-array (tabs term) columns
                                                 :initial-element 0))

    ;; resize each row to new width, zero-pad if needed
    (loop for i below minrow
          do (flet ((r (s)
                      (setf (aref s i)
                            (adjust-array (aref s i) columns))
                      (loop for j from mincol below columns
                            do (setf (aref (aref s i) j)
                                     (make-instance 'glyph)))))
               (r (screen term))
               (r (alternate-screen term))))
    ;; allocate any new rows
    (loop for i from minrow below rows
          do (flet ((n (s)
                      (setf (aref s i)
                            (make-array rows
                                        :element-type '(vector glyph *)
                                        :initial-contents
                                        (coerce
                                         (loop repeat columns
                                               collect (make-instance 'glyph))
                                         '(vector glyph))))))
               (n (screen term))
               (n (alternate-screen term))))
    (when (> columns (columns term))
      (loop with last-tab = (position 1 (tabs term) :from-end t)
            for i from (+ *tab-spaces* (or last-tab 0))
              below columns by *tab-spaces*
            do (setf (aref (tabs term) i) 1)))
    ;; update terminal size
    (setf (slot-value term 'columns) columns
          (slot-value term 'rows) rows)
    ;; reset scrolling region
    (tsetscroll 0 (1- rows) :term term)
    ;; make use of the LIMIT in tmoveto
    (tmoveto (x (cursor term)) (y (cursor term)))
    ;; Clearing both screens (it makes dirty all lines)
    (let ((c (cursor term)))
      (loop repeat 2
            do (when (and (< mincol columns) (< 0 minrow))
                 (tclearregion mincol 0 (1- columns) (1- minrow) :term term))
               (when (and (< 0 columns) (< minrow rows))
                 (tclearregion 0 minrow (1- columns) (1- rows) :term term))
               (tswapscreen :term term)
               (tcursor :cursor-load :term term))
      (setf (cursor term) c))))

