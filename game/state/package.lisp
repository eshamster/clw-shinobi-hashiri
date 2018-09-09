(defpackage clw-shinobi-hashiri/game/state/package
  (:use :cl
        :ps-experiment
        ;; The followings are required to make package-inferred-system to recognize them
        :clw-shinobi-hashiri/game/state/global-init
        :clw-shinobi-hashiri/game/state/main
        :clw-shinobi-hashiri/game/state/menu)
  (:export :init-clw-shinobi-hashiri-state)
  (:import-from :cl-web-2d-game
                :init-game-state
                :make-state))
(in-package :clw-shinobi-hashiri/game/state/package)

(defun.ps+ init-clw-shinobi-hashiri-state ()
  (init-game-state (make-state :global-init)))
