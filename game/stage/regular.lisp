(defpackage clw-shinobi-hashiri/game/stage/regular
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-random-stage-info)
  (:import-from :clw-shinobi-hashiri/game/stage/utils
                :make-wall
                :make-hole
                :wall-height

                :stage-info
                :fn-get-wall
                :fn-get-scroll-speed
                :fn-process

                :wall-is-hole-p))
(in-package :clw-shinobi-hashiri/game/stage/regular)

;; --- random stage generator --- ;;

(defvar.ps+ *regular-generator-params*
    (convert-to-layered-hash
     (:height (:min #ly50 :max #ly800
               :diff (:min #ly50 :max #ly500))
      :width (:min #lx70 :max #lx400)
      :hole (:ratio 0.3 :min #lx80 :max #lx300)
      :scroll (:first #lx4 :max #lx9 :accell #lx0.1))))

(defvar.ps+ *needle-generator-params*
    (convert-to-layered-hash
     (:height (:min #ly300 :max #ly800
               :diff (:min #ly100 :max #ly500))
      :width (:min #lx30 :max #lx30)
      :hole (:ratio 1 :min #lx150 :max #lx350)
      :scroll (:first #lx4 :max #lx8.5 :accell #lx0.005))))

(defvar.ps+ *big-hole-generator-params*
    (convert-to-layered-hash
     (:height (:min #ly200 :max #ly900
               :diff (:min #ly100 :max #ly600))
      :width (:min #lx200 :max #lx600)
      :hole (:ratio 0.6 :min #lx300 :max #lx800)
      :scroll (:first #lx6 :max #lx10 :accell #lx0.01))))

(defstruct.ps+
    (random-stage-info
     (:include stage-info
               (fn-get-wall #'get-wall-randomly)
               (fn-get-scroll-speed #'get-scroll-speed)
               (fn-process #'process-regular-stage)))
    pre-wall
  (pre-is-hole-p nil)
  random-params
  scroll-speed)

(defun.ps-only random1 () (random))
(defun random1 () (random 1.0))

(defmacro.ps+ get-param-randomly (params key)
  (flet ((get-my-param (key1 key2)
           `(get-layered-hash ,params ,key1 ,key2)))
    `(lerp-scalar ,(get-my-param key :min)
                  ,(get-my-param key :max)
                  (random1))))

(defun.ps+ calc-next-height (params pre-wall)
  (let ((pre-height (wall-height pre-wall)))
    (labels ((rec ()
               (let ((height (get-param-randomly params :height)))
                 (if (and (> (abs (- pre-height height))
                               (get-layered-hash params :height :diff :min))
                            ;; Intentionally omit "abs"
                            (< (- height pre-height)
                               (get-layered-hash params :height :diff :max)))
                     (return-from rec height)
                     (rec)))))
      (rec))))

(defun.ps+ get-wall-randomly (id info)
  (declare (ignore id))
  (check-type info random-stage-info)
  (with-slots (pre-wall (params random-params) pre-is-hole-p) info
    (let ((result
           (cond ((null pre-wall)
                  (make-wall :height #ly600 :width #lx800))
                 ((and (not pre-is-hole-p)
                       (> (get-layered-hash params :hole :ratio)
                          (random1)))
                  (make-hole :width (get-param-randomly params :hole)))
                 (t (make-wall :height (calc-next-height params pre-wall)
                               :width (get-param-randomly params :width))))))
      (if (wall-is-hole-p result)
          (setf pre-is-hole-p t)
          (progn (setf pre-wall result
                       pre-is-hole-p nil)))
      result)))

(defun.ps+ get-scroll-speed (info)
  (random-stage-info-scroll-speed info))

(defun.ps+ process-regular-stage (info)
  (symbol-macrolet ((speed (random-stage-info-scroll-speed info)))
    (let ((params (random-stage-info-random-params info)))
      (setf speed
            (min (+ speed (get-layered-hash params :scroll :accell))
                 (get-layered-hash params :scroll :max))))))

(defun.ps+ init-random-stage-info (stage-name)
  (let ((params (ecase stage-name
                   (:regular *regular-generator-params*)
                   (:needle *needle-generator-params*)
                   (:big-hole *big-hole-generator-params*))))
    (let ((info (make-random-stage-info :random-params params)))
      (setf (random-stage-info-scroll-speed info)
            (get-layered-hash params :scroll :first))
      info)))
