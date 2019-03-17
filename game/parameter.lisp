(defpackage clw-shinobi-hashiri/game/parameter
  (:use :cl
        :ps-experiment
        :cl-web-2d-game)
  (:export :get-param
           :get-depth
           :get-state-param))
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
              :death-margin (:x-ratio 2.0 :y-ratio 1.0)
              ;; - state params - ;;
              :jump (:speed 5 :max-time 18)
              :on-ground (:default-x #lx300 :return-speed #lx5)
              ;; TODO: Speed to y direction should be specified by #ly instead of #lx
              :glide (:gravity-rate 0.15 :first-y-speed #lx2.1 :max-fall-speed #lx3.5
                      :back-speed #lx0.4)
              :glide-after-climb (:x-speed #lx0.8 :y-speed #ly4
                                  :adding-scale-y-speed-to-x 0.1)
              :climb-jump (:min-speed #lx10 :max-speed #lx8 :duration 20)
              :climb (:scroll-speed-scale 0.8))
    :gravity (:accell #ly0.45 :max-speed 20)
    :score-board (:x #lx990 :y #ly990
                  :current-score (:font-size 20 :y #ly-35)
                  :best-score (:font-size 12 :y #ly0)))))

(defmacro.ps+ get-param (&rest keys)
  `(get-layered-hash *params* ,@keys))

(defvar.ps+ *depth*
    (convert-to-layered-hash
     (:field 0
      :ground 2
      :shinobi 10)))

(defmacro.ps+ get-depth (&rest keys)
  `(get-layered-hash *depth* ,@keys))

(defvar.ps+ *state-params*
  (convert-to-layered-hash
   (:menu (:logo (:x #lx20 :y #ly500
                  :width #lx800)
           :selector (:x #lx240 :y #ly350
                      :item (:text-size 30
                             :height #ly80)
                      :cursor (:offset-x #lx10
                               :width #lx15 :height #lx30))
           :press-key-info (:text-size 20
                            :x #lx50 :y #ly80)))))

(defmacro.ps+ get-state-param (&rest keys)
  `(get-layered-hash *state-params* ,@keys))
