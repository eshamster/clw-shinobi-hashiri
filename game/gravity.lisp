(defpackage clw-shinobi-hashiri/game/gravity
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :gravity
           :make-gravity
           :gravity-system
           :make-gravity-system

           :start-gravity
           :stop-gravity)
  (:import-from :clw-shinobi-hashiri/game/parameter
                :get-param
                :get-depth))
(in-package :clw-shinobi-hashiri/game/gravity)

(defstruct.ps+ (gravity (:include ecs-component))
    (on-p t))

(defun.ps+ set-gravity-enable (entity value)
  (with-ecs-components (gravity) entity
    (setf (gravity-on-p gravity) value)))

(defun.ps+ start-gravity (entity)
  (set-gravity-enable entity t))

(defun.ps+ stop-gravity (entity)
  (set-gravity-enable entity nil))

(defun.ps+ process-gravity (entity)
  (with-ecs-components (gravity speed-2d) entity
    (when (gravity-on-p gravity)
      (let ((accell (get-param :gravity :accell))
            (max-speed (get-param :gravity :max-speed))
            (speed-y (speed-2d-y speed-2d)))
        (decf speed-y accell)
        (when (< speed-y (* -1 max-speed))
          (setf speed-y (* -1 max-speed)))
        (setf (speed-2d-y speed-2d) speed-y)))))

(defstruct.ps+
    (gravity-system
     (:include ecs-system
               (target-component-types '(gravity point-2d speed-2d))
               (process #'process-gravity))))
