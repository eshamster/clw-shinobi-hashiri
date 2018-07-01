(defpackage clw-shinobi-hashiri/game/game
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-func
           :update-func)
  (:import-from :clw-shinobi-hashiri/game/state/package
                :init-clw-shinobi-hashiri-state))
(in-package :clw-shinobi-hashiri/game/game)

(defun.ps+ init-func (scene)
  (init-clw-shinobi-hashiri-state)
  (init-default-systems :scene scene)
  (init-input))

(defun.ps+ update-func ()
  (process-game-state))
