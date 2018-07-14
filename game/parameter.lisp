(defpackage clw-shinobi-hashiri/game/parameter
  (:use :cl
        :ps-experiment
        :cl-web-2d-game)
  (:export :get-param
           :get-depth))
(in-package :clw-shinobi-hashiri/game/parameter)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defvar.ps+ field-height 600)
  (defvar.ps+ field-width (/ (* field-height 4.0) 3))

  (defun.ps+ calc-absolute-length (relative-length base-length)
    (* relative-length base-length 0.001))

  "#Ex1. '#lx500' represents a half length of the field width."
  "#Ex2. '#ly500' represents a half length of the field height."
  (set-dispatch-macro-character
   #\# #\l
   #'(lambda (stream &rest rest)
       (declare (ignore rest))
       (case (peek-char nil stream)
         (#\x (read-char stream)
              `(calc-absolute-length ,(read stream) field-width))
         (#\y (read-char stream)
              `(calc-absolute-length ,(read stream) field-height))
         (t (error "Not recognized character after #l"))))))

(defvar.ps+ *params*
  (convert-to-layered-hash
   (:field (:width field-width :height field-height)
    :shinobi (:width #ly40 :height #ly40
              :jump (:speed 4 :max-time 20))
    :gravity (:accell #ly0.4 :max-speed 20))))

(defmacro.ps+ get-param (&rest keys)
  `(get-layered-hash *params* ,@keys))

(defvar.ps+ *depth*
    (convert-to-layered-hash
     (:field 0
      :ground 2
      :shinobi 10)))

(defmacro.ps+ get-depth (&rest keys)
  `(get-layered-hash *depth* ,@keys))
