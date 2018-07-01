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

(defun.ps+ init-shinobi (parent)
  (let ((shinobi (make-ecs-entity)))
    (add-entity-tag shinobi :shinobi)
    (add-ecs-component-list
     shinobi
     (make-point-2d :x 100 :y 100)
     (make-gravity)
     (make-speed-2d :y 5)
     (make-model-2d :model (make-solid-rect :width (get-param :shinobi :width)
                                            :height (get-param :shinobi :height)
                                            :color #x000000)
                    :depth (get-depth :shinobi)))
    (add-ecs-entity shinobi parent)))
