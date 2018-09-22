(defpackage clw-shinobi-hashiri/game/score-board
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :update-score-board
           :finalize-current-score
           :init-score-board)
  (:import-from :clw-shinobi-hashiri/game/parameter
                :get-param))
(in-package :clw-shinobi-hashiri/game/score-board)

(defun.ps+ find-score-board ()
  (let ((board (find-a-entity-by-tag :score-board)))
    (assert board)
    board))

(defun.ps+ calc-score (&key moved-length)
  (floor (/ moved-length 50)))

(defun.ps+ update-score-board (&key moved-length
                                    (board (find-score-board)))
  (check-entity-tags board :score-board)
  (let ((prev-score (get-entity-param board :current-score))
        (new-score (calc-score :moved-length moved-length)))
    (unless (= prev-score new-score)
      (set-entity-param board :current-score new-score)
      (update-child-score-board
       (get-entity-param board :current-score-board)
       new-score))))

(defun.ps+ finalize-current-score (&optional (board (find-score-board)))
  (check-entity-tags board :score-board)
  (aset-entity-param board :best-score
                     (max it (get-entity-param board :current-score)))
  (set-entity-param board :current-score 0)
  (update-child-score-board
   (get-entity-param board :current-score-board) 0)
  (update-child-score-board
   (get-entity-param board :best-score-board)
   (get-entity-param board :best-score)))

(defun.ps+ update-child-score-board (child-board score)
  (clear-text-area child-board)
  (add-text-to-area child-board
                    :text (+ (get-entity-param child-board :prefix)
                             score)
                    :color #x000000))

(defun.ps+ make-child-score-board (&key (prefix "") font-size y)
  (let ((board (make-text-area :font-size font-size
                               :text-align :right
                               :x 0 :y y)))
    (add-ecs-component-list
     board
     (init-entity-params :prefix prefix))
    (update-child-score-board board 0)
    board))

(defun.ps+ make-current-score-board ()
  (make-child-score-board :font-size (get-param :score-board :current-score :font-size)
                          :y (get-param :score-board :current-score :y)))

(defun.ps+ make-best-score-board ()
  (make-child-score-board :prefix "Best: "
                          :font-size (get-param :score-board :best-score :font-size)
                          :y (get-param :score-board :best-score :y)))

(defun.ps+ init-score-board ()
  (let ((board (make-ecs-entity))
        (current-score-board (make-current-score-board))
        (best-score-board (make-best-score-board)))
    (add-entity-tag board :score-board)
    (add-ecs-component-list
     board
     (make-point-2d :x (get-param :score-board :x)
                    :y (get-param :score-board :y))
     (init-entity-params :current-score 0
                         :best-score 0
                         :current-score-board current-score-board
                         :best-score-board best-score-board))
    (add-ecs-entity board)
    (add-ecs-entity current-score-board board)
    (add-ecs-entity best-score-board board)))
