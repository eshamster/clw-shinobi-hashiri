(defpackage clw-shinobi-hashiri/game/ground
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-ground
           :get-ground-height)
  (:import-from :clw-shinobi-hashiri/game/parameter
                :get-param
                :get-depth))
(in-package :clw-shinobi-hashiri/game/ground)

(defvar.ps+ *dummy-height* 60)

(defun.ps+ get-ground-height (center-x width)
  (declare (ignore center-x width))
  *dummy-height*)

(defun.ps+ init-ground (parent)
  (let ((ground (make-ecs-entity)))
    (add-ecs-component-list
     ground
     (make-point-2d)
     (make-model-2d :model (make-solid-rect :width (get-param :field :width)
                                            :height *dummy-height*
                                            :color #x000000)
                    :depth (get-depth :ground)))
    (add-ecs-entity ground parent)))
