(defpackage clw-shinobi-hashiri/game/controller
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-controller
           :control-by-mouse)
  (:import-from :clw-shinobi-hashiri/game/shinobi
                :press-action-key
                :release-action-key))
(in-package :clw-shinobi-hashiri/game/controller)

(defun.ps+ control-by-mouse ()
  (case (get-left-mouse-state)
    (:up-now (release-action-key))
    (:down-now (press-action-key))))

(defun.ps+ control-by-touch ()
  (case (get-total-touch-state)
    (:up-now (release-action-key)
             (add-to-event-log "released"))
    (:down-now (press-action-key)
               (add-to-event-log "pressed"))))

(defun.ps+ control-by-keyboard ()
  (dolist (key (list :a :b))
    (when (is-key-up-now key)
      (release-action-key))
    (when (is-key-down-now key)
      (press-action-key))))

(defun.ps+ process-controller (entity)
  (declare (ignore entity))
  (control-by-keyboard)
  (control-by-mouse)
  (control-by-touch))

(defun.ps+ init-controller ()
  (let ((controller (make-ecs-entity)))
    (add-ecs-component-list
     controller
     (make-script-2d :func #'process-controller))
    (add-ecs-entity controller)))
