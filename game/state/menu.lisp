(defpackage clw-shinobi-hashiri/game/state/menu
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-shinobi-hashiri/game/ground
                :get-stage-kind-list))
(in-package :clw-shinobi-hashiri/game/state/menu)

;; --- stage selector --- ;;

(defstruct.ps+ stage-selector current-index)

(defun.ps+ init-stage-selector ()
  (make-stage-selector :current-index 0))

(defun.ps+ get-stage-kind (selector)
  (with-slots ((index current-index)) selector
    (let ((lst (get-stage-kind-list)))
      (assert (and (>= index 0)
                   (< index (length lst))))
      (nth (stage-selector-current-index selector)
           lst))))

(defun.ps+ select-next-stage (selector)
  (with-slots ((index current-index)) selector
    (incf index)
    (when (>= index (length (get-stage-kind-list)))
      (setf index 0))))

(defun.ps+ select-previous-stage (selector)
  (with-slots ((index current-index)) selector
    (decf index)
    (when (< index 0)
      (setf index (1- (length (get-stage-kind-list)))))))

(defun.ps+ update-selector-display (selector)
  ;; TODO: Visualize.
  (add-to-event-log (+ "Stage: " (get-stage-kind selector))))

(defun.ps+ process-selector (selector)
  (when (is-key-down-now :right)
    (select-next-stage selector)
    (update-selector-display selector))
  (when (is-key-down-now :left)
    (select-previous-stage selector)
    (update-selector-display selector)))

(defun.ps+ add-logo ()
  (frame-promise-then
   (make-texture-model-promise :width #lx800 :texture-name "logo")
   (lambda (model)
     (let ((logo (make-ecs-entity)))
       (add-ecs-component-list
        logo
        (make-point-2d :x #lx20 :y #ly500)
        (make-model-2d :model model))
       (add-ecs-entity logo)))))

;; --- state --- ;;

(def-game-state menu ((parent (make-ecs-entity))
                      (selector (init-stage-selector)))
  :start-process
  (state-lambda (parent)
    (with-ecs-entity-parent (parent)
      (let ((area (make-text-area :font-size 20
                                  :text-align :right)))
        (add-text-to-area area
                          :text "Press z key to start ..."
                          :color #xffffff)
        (add-ecs-component-list
         area
         (make-point-2d :x #lx980 :y #ly80))
        (add-ecs-entity area))
      (add-logo))
    t)
  :process
  (state-lambda (selector)
    (process-selector selector)
    (when (is-key-down-now :a)
      (make-state :main
                  :stage-kind (get-stage-kind selector))))
  :end-process
  (state-lambda (parent)
    (register-next-frame-func
     (lambda () (delete-ecs-entity parent)))
    t))
