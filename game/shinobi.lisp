(defpackage clw-shinobi-hashiri/game/shinobi
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-shinobi)
  (:import-from :clw-shinobi-hashiri/game/gravity
                :make-gravity)
  (:import-from :clw-shinobi-hashiri/game/parameter
                :get-param
                :get-depth))
(in-package :clw-shinobi-hashiri/game/shinobi)

;; --- utils --- ;;

(defun.ps+ find-shinobi ()
  (find-a-entity-by-tag :shinobi))

;; --- jump --- ;;

;; TODO: Split controller

(defvar.ps+ *jump-key* :a)

(defstruct.ps+ (shinobi-state (:include game-state)) shinobi)

(defstruct.ps+
    (jumping-state
     (:include shinobi-state
               (start-process (lambda (state)
                                (let ((shinobi (shinobi-state-shinobi state)))
                                  (set-entity-param shinobi :on-ground-p nil))
                                t))
               (process (lambda (state)
                          (let ((shinobi (shinobi-state-shinobi state)))
                            (set-entity-param shinobi :on-ground-p nil)
                            (setf (speed-2d-y (get-ecs-component 'speed-2d shinobi))
                                  (get-param :shinobi :jump :speed))
                            (symbol-macrolet ((duration (jumping-state-duration state)))
                              (decf duration)
                              (when (or (<= duration 0)
                                        (is-key-up-now *jump-key*))
                                (make-falling-state :shinobi shinobi))))))))
  (duration (get-param :shinobi :jump :max-time)))

(defstruct.ps+
    (falling-state
     (:include shinobi-state
               (process (lambda (state)
                          (let ((shinobi (shinobi-state-shinobi state)))
                            (when (get-entity-param shinobi :on-ground-p)
                              (make-on-ground-state :shinobi shinobi))))))))

(defstruct.ps+
    (on-ground-state
     (:include shinobi-state
               (process (lambda (state)
                          (when (is-key-down-now *jump-key*)
                            (make-jumping-state :shinobi (shinobi-state-shinobi state))))))))

(defun.ps+ process-jump-state (shinobi)
  (check-entity-tags shinobi :shinobi)
  (process-game-state (get-entity-param shinobi :jump-state-manager)))

(defun.ps+ debug-print-state (shinobi)
  (add-to-monitoring-log
   (+ "shinobi jump state: "
      (etypecase (cl-web-2d-game/core/game-state::game-state-manager-current-state
                  (get-entity-param shinobi :jump-state-manager))
        (jumping-state "jumping")
        (falling-state "falling")
        (on-ground-state "on-ground")))))

;; --- main --- ;;

(defun.ps+ init-shinobi (parent)
  (let ((shinobi (make-ecs-entity))
        (width (get-param :shinobi :width))
        (height (get-param :shinobi :height)))
    (add-entity-tag shinobi :shinobi)
    (flet ((get-point (entity)
             (get-ecs-component 'point-2d entity)))
      (add-ecs-component-list
       shinobi
       (make-point-2d :x 100 :y 100)
       (make-gravity :fn-get-width (lambda (entity)
                                     (declare (ignore entity))
                                     width)
                     :fn-get-center-x (lambda (entity)
                                        (point-2d-x (get-point entity)))
                     :fn-get-bottom (lambda (entity)
                                      (+ (point-2d-y (get-point entity))
                                         (* -1/2 height)))
                     :fn-on-ground (lambda (entity)
                                     (set-entity-param entity :on-ground-p t)))
       (make-speed-2d :y 0)
       (make-model-2d :model (make-solid-rect :width width
                                              :height height
                                              :color #x000000)
                      :depth (get-depth :shinobi)
                      :offset (make-point-2d :x (* -1/2 width)
                                             :y (* -1/2 height)))
       (make-script-2d :func (lambda (entity)
                               (process-jump-state entity)
                               (debug-print-state entity)))
       (init-entity-params :jump-state-manager
                           (init-game-state-manager (make-falling-state :shinobi shinobi))
                           :jump-input-state :up ;; up-now up down-now down
                           :on-ground-p nil)))
    (add-ecs-entity shinobi parent)))
