# 3bst
CL port of the terminal emulation part of st (http://st.suckless.org/)

Not very well tested, but seems to work well enough to run emacs for a bit.

general usage (probably missing some details):

```lisp

(let ((term (make-instance '3bst:term :rows 10 :columns 40)))

  (3bst:handle-input (format nil "testing... ~c[31mRed~c[32mGreen~c[34mBlue~c[33m!"
                             (code-char 27) (code-char 27) (code-char 27) (code-char 27)) ;; (code-char 27) = esc
                     :term term)

  (loop with dirty = (3bst:dirty term);;bitvector of 'dirty' flag for each row
        for row below (3bst:rows term)
        do (format t "~,' 2d~a:" row (if (plusp (aref dirty row)) "*" " "))
           (loop for col below (3bst:columns term)
                 for glyph = (3bst:glyph-at (3bst::screen term) row col)
                 for char = (3bst:c glyph)
                 do (format t "~a" char))
           (format t "~%"))

  (loop for row below (3bst:rows term)
        do (format t "~,' 2d :" row)
           (loop for col below (3bst:columns term)
                 for glyph = (3bst:glyph-at (3bst::screen term) row col)
                 for fg = (3bst:color-rgb (3bst:fg glyph))
                 do (format t "~a" (destructuring-bind (r g b) fg
                                     (cond
                                       ((> r (max g b)) "r")
                                       ((> g (max r b)) "g")
                                       ((> b (max r g)) "b")
                                       ((< b (min r g)) "y")
                                       (t " ")))))
           (format t "~%")))

;; ->
;;
;; 0*:testing... RedGreenBlue!                
;; 1 :                                        
;; 2 :                                        
;; 3 :                                        
;; 4 :                                        
;; 5 :                                        
;; 6 :                                        
;; 7 :                                        
;; 8 :                                        
;; 9 :                                        
;; 0 :           rrrgggggbbbby                
;; 1 :                                        
;; 2 :                                        
;; 3 :                                        
;; 4 :                                        
;; 5 :                                        
;; 6 :                                        
;; 7 :                                        
;; 8 :                                        
;; 9 :                                        

```

probably will want to use it with something like `SB-EXT:RUN-PROGRAM`, something like:


```lisp

(let* ((term (make-instance '3bst:term :rows 12 :columns 51))
      ;; start ssh in a subprocess, with input and output as streams
      (proc (sb-ext:run-program "C:/Program Files (x86)/PuTTY/plink.exe"
                                (list "192.168.0.1")
                                :wait nil
                                :output :stream
                                :input :stream
                                :external-format :utf-8))
       (stop nil)
       thread)
  ;; start a thread reading from the process and updating the term
  (setf thread
        (sb-thread:make-thread
         (lambda ()
           (loop for c = (read-char-no-hang (sb-ext:process-output proc)
                                            nil :eof)
                 until (eq c :eof)
                 when c
                   do (3bst:handle-input (string c) :term term)
                 until stop
                 unless c
                   do (sleep 0.01)))))
  ;; send some input to subprocess
  (format (sb-ext:process-input proc) "screen -xRR test emacs -nw~%")
  (finish-output (sb-ext:process-input proc))
  (sleep 1) ;; wait for it to respond
  (setf stop t) ;; exit the thread
  ;; print the output
  (loop with dirty = (3bst:dirty term)
        for row below (3bst:rows term)
        do (format t "~,' 2d~a:" row (if (plusp (aref dirty row)) "*" " "))
           (loop for col below (3bst:columns term)
                 for glyph = (3bst:glyph-at (3bst::screen term) row col)
                 for char = (3bst:c glyph)
                 do (format t "~a" char))
           (format t "~%"))
  ;; send some more input to exit screen and close shell
  (format (sb-ext:process-input proc) (format nil "~cd~c" (code-char 1) (code-char 4))) ;; ^Ad^D
  (finish-output (sb-ext:process-input proc))
  (sb-thread:join-thread thread))

;; ->

;;  0*:Welcome to GNU Emacs, one component of the GNU/Li |
;;  1*:nux operating system.                             |
;;  2*:To follow a link, click Mouse-1 on it, or move to |
;;  3*: it and type RET.                                 |
;;  4*:To quit a partially entered command, type Control |
;;  5*:-g.                                               |
;;  6*:                                                  |
;;  7*:Important Help menu items:                        |
;;  8*:-UUU:%%--F1  *GNU Emacs*    Top L3     (Fundamenta|
;;  9*:                                                  |
;; 10*:-------------------------------------------------- 
;; 11*:                                                   

```

