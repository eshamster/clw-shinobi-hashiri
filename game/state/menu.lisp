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

;; --- state --- ;;

(def-game-state menu ((parent (make-ecs-entity))
                      (selector (init-stage-selector)))
  :start-process
  (lambda (_this)
    (add-ecs-entity (slot-value _this 'parent))
    ;; TODO: Visualize.
    (add-to-event-log "Push z key to start.")
    t)
  :process
  (lambda (_this)
    (let ((selector (slot-value _this 'selector)))
      (process-selector selector)
      (when (is-key-down-now :a)
        (make-state :main
                    :stage-kind (get-stage-kind selector)))))
  :end-process
  (lambda (_this)
    (register-next-frame-func
     (lambda () (delete-ecs-entity (slot-value _this 'parent))))
    t))
