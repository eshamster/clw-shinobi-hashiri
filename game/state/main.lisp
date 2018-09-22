(defpackage clw-shinobi-hashiri/game/state/main
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-shinobi-hashiri/game/ground
                :init-ground)
  (:import-from :clw-shinobi-hashiri/game/parameter
                :get-param
                :get-depth)
  (:import-from :clw-shinobi-hashiri/game/score-board
                :init-score-board)
  (:import-from :clw-shinobi-hashiri/game/shinobi
                :init-shinobi))
(in-package :clw-shinobi-hashiri/game/state/main)

(def-game-state main ((parent (make-ecs-entity)) stage-kind)
  :start-process
  (state-lambda (parent stage-kind)
    (with-ecs-entity-parent (parent)
      (let ((background (make-ecs-entity)))
        (add-ecs-component-list
         background
         (make-point-2d :x 0 :y 0)
         (make-model-2d :model (make-solid-rect :width (get-param :field :width)
                                                :height (get-param :field :height)
                                                :color #xeeeeee)
                        :depth (get-depth :field)))
        (add-ecs-entity background)
        (init-score-board)
        (let ((ground (init-ground stage-kind)))
          (init-shinobi ground))))
    t)
  :process
  (state-lambda ()
    (when (is-key-up-now :escape)
      (make-state :menu)))
  :end-process
  (state-lambda (parent)
    (register-next-frame-func
     (lambda () (delete-ecs-entity parent)))
    t))
