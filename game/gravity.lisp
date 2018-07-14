(defpackage clw-shinobi-hashiri/game/gravity
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :gravity
           :make-gravity
           :gravity-decrease-rate

           :gravity-system
           :make-gravity-system

           :start-gravity
           :stop-gravity)
  (:import-from :clw-shinobi-hashiri/game/ground
                :get-ground-height)
  (:import-from :clw-shinobi-hashiri/game/parameter
                :get-param
                :get-depth))
(in-package :clw-shinobi-hashiri/game/gravity)

(defstruct.ps+ (gravity (:include ecs-component))
    (on-p t)
  (decrease-rate 1.0)
  (fn-get-center-x (lambda (entity) (declare (ignore entity)) 0))
  (fn-get-bottom (lambda (entity) (declare (ignore entity)) 0))
  (fn-get-width (lambda (entity) (declare (ignore entity)) 0))
  (fn-on-ground (lambda (entity) (declare (ignore entity)))))

(defun.ps+ set-gravity-enable (entity value)
  (with-ecs-components (gravity) entity
    (setf (gravity-on-p gravity) value)))

(defun.ps+ start-gravity (entity)
  (set-gravity-enable entity t))

(defun.ps+ stop-gravity (entity)
  (set-gravity-enable entity nil))

(defun.ps+ stop-if-on-ground (entity gravity speed-2d)
  (with-slots (fn-get-center-x fn-get-bottom fn-get-width fn-on-ground) gravity
    (let ((ground-height (get-ground-height
                          (funcall fn-get-center-x entity)
                          (funcall fn-get-width entity)))
          (bottom (funcall fn-get-bottom entity)))
      (when (< bottom ground-height)
        (setf (speed-2d-y speed-2d) 0)
        (with-ecs-components (point-2d) entity
          (setf (point-2d-y point-2d)
                (+ (point-2d-y point-2d)
                   (- ground-height bottom))))
        ;; XXX: Should not call hook in sequential frames
        (funcall fn-on-ground entity)))))

(defun.ps+ process-gravity (entity)
  (with-ecs-components (gravity speed-2d) entity
    (when (gravity-on-p gravity)
      (let ((accell (get-param :gravity :accell))
            (max-speed (get-param :gravity :max-speed))
            (speed-y (speed-2d-y speed-2d)))
        (decf speed-y (* accell
                         (gravity-decrease-rate gravity)))
        (when (< speed-y (* -1 max-speed))
          (setf speed-y (* -1 max-speed)))
        (setf (speed-2d-y speed-2d) speed-y))
      (stop-if-on-ground entity gravity speed-2d))))

(defstruct.ps+
    (gravity-system
     (:include ecs-system
               (target-component-types '(gravity point-2d speed-2d))
               (process #'process-gravity))))
