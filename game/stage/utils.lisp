(defpackage clw-shinobi-hashiri/game/stage/utils
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-wall
           :make-hole
           :wall
           :wall-id
           :wall-width
           :wall-height
           :clone-wall

           :stage-info
           :stage-info-fn-get-wall
           :fn-get-wall
           :stage-info-fn-get-scroll-speed
           :fn-get-scroll-speed
           :stage-info-fn-process
           :fn-process

           :wall-is-hole-p))
(in-package :clw-shinobi-hashiri/game/stage/utils)

;; --- stage structure --- ;;

;; - wall - ;;

(defstruct.ps+ (wall (:include ecs-component))
    id height width name)

(defun.ps+ clone-wall (wall)
  (with-slots (id height width name) wall
    (make-wall :id id :height height :width width :name name)))

(defun.ps+ make-hole (&key width)
  (make-wall :width width
             :height -1))

(defun.ps+ wall-is-hole-p (wall-cmp)
  (< (wall-height wall-cmp) 0))

;; - stage-info - ;;

(defstruct.ps+ stage-info
    (fn-get-wall (lambda (id info) (declare (ignore id info))))
  (fn-get-scroll-speed (lambda (info) (declare (ignore info))))
  (fn-process (lambda (info) (declare (ignore info)))))
