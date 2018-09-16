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
  (state-lambda ()
    (load-font "js/")
    (register-ecs-system :gravity (make-gravity-system))
    (start-key-monitoring :escape "escape")
    t)
  :process
  (state-lambda ()
    (make-state :menu)))
