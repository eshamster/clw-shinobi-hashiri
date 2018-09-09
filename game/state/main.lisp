(defpackage clw-shinobi-hashiri/game/state/main
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-shinobi-hashiri/game/ground
                :init-ground)
  (:import-from :clw-shinobi-hashiri/game/shinobi
                :init-shinobi)
  (:import-from :clw-shinobi-hashiri/game/parameter
                :get-param
                :get-depth))
(in-package :clw-shinobi-hashiri/game/state/main)

(def-game-state main ((parent (make-ecs-entity)) stage-kind)
  :start-process
  (lambda (_this)
    (with-ecs-entity-parent ((slot-value _this 'parent))
      (let ((background (make-ecs-entity)))
        (add-ecs-component-list
         background
         (make-point-2d :x 0 :y 0)
         (make-model-2d :model (make-solid-rect :width (get-param :field :width)
                                                :height (get-param :field :height)
                                                :color #xeeeeee)
                        :depth (get-depth :field)))
        (add-ecs-entity background)
        (let ((ground (init-ground (slot-value _this 'stage-kind))))
          (init-shinobi ground))))
    t)
  :process
  (lambda (_this)
    (declare (ignore _this))
    nil))
