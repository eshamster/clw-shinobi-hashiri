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

(def-game-state main ((parent (make-ecs-entity)))
  :start-process
  (lambda (_this)
    (let ((parent (slot-value _this 'parent)))
      (add-ecs-entity parent)
      (init-shinobi parent)
      (let ((background (make-ecs-entity)))
        (add-ecs-component-list
         background
         (make-point-2d :x 0 :y 0)
         (make-model-2d :model (make-solid-rect :width (get-param :field :width)
                                                :height (get-param :field :height)
                                                :color #xeeeeee)
                        :depth (get-depth :field)))
        (add-ecs-entity background parent)
        (init-ground parent)))
    t)
  :process
  (lambda (_this)
    (declare (ignore _this))
    nil))
