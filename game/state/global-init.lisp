(defpackage clw-shinobi-hashiri/game/state/global-init
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-shinobi-hashiri/game/gravity
                :make-gravity-system))
(in-package :clw-shinobi-hashiri/game/state/global-init)

(def-game-state global-init ()
  :start-process
  (lambda (_this)
    (declare (ignore _this))
    (load-font "js/")
    (register-ecs-system :gravity (make-gravity-system))
    t)
  :process
  (lambda (_this)
    (declare (ignore _this))
    (make-state :main))
  :end-process
  (lambda (_this)
    t))
