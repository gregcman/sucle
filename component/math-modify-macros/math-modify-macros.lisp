(defpackage :math-modify-macros
  (:use #:cl)
  (:export
   #:*=
   #:+=
   #:-=
   #:/f
   #:&=
   #:^=
   #:|\|=|
   #:<<=
   #:>>=
   #:hsa
   #:logiorf))
(in-package :math-modify-macros)

(define-modify-macro *= (&rest args) *)
(define-modify-macro += (&rest args) +)
(define-modify-macro -= (&rest args) -)
(define-modify-macro /f (&rest args) /)
(define-modify-macro &= (&rest args) logand)
(define-modify-macro ^= (&rest args) logxor)
(define-modify-macro |\|=| (&rest args) logior)
(define-modify-macro <<= (&rest args) ash)
(define-modify-macro >>= (&rest args) hsa)
(defmacro hsa (a b)
  `(ash ,b ,a))

(define-modify-macro logiorf (&rest args) logior)
