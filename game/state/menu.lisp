(defpackage clw-shinobi-hashiri/game/state/menu
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-shinobi-hashiri/game/ground
                :get-stage-kind-list)
  (:import-from :clw-shinobi-hashiri/game/parameter
                :get-state-param))
(in-package :clw-shinobi-hashiri/game/state/menu)

(defmacro.ps+ get-menu-param (&rest keys)
  `(get-state-param :menu ,@keys))

;; --- stage selector --- ;;

(defstruct.ps+ (stage-selector (:include ecs-component))
    current-index (on-stage-change (list)))

(defun.ps+ init-stage-selector ()
  (make-stage-selector :current-index 0))

(defun.ps+ get-stage-kind (selector)
  (with-slots ((index current-index)) selector
    (let ((lst (get-stage-kind-list)))
      (assert (and (>= index 0)
                   (< index (length lst))))
      (nth (stage-selector-current-index selector)
           lst))))

(defun.ps+ select-next-stage (selector)
  (with-slots ((index current-index)) selector
    (incf index)
    (when (>= index (length (get-stage-kind-list)))
      (setf index 0))))

(defun.ps+ select-previous-stage (selector)
  (with-slots ((index current-index)) selector
    (decf index)
    (when (< index 0)
      (setf index (1- (length (get-stage-kind-list)))))))

(defun.ps+ add-on-change-stage (fn selector)
  "Signature: (lambda (new-index) ...)"
  (push fn (stage-selector-on-stage-change selector)))

(defun.ps+ call-on-change-stage (selector)
  (dolist (fn (stage-selector-on-stage-change selector))
    (funcall fn (stage-selector-current-index selector))))

(defun.ps+ process-selector (selector-entity)
  (check-entity-tags selector-entity :selector)
  (with-ecs-components ((selector stage-selector)) selector-entity
    (when (key-down-now-p :down)
      (select-next-stage selector)
      (call-on-change-stage selector))
    (when (key-down-now-p :up)
      (select-previous-stage selector)
      (call-on-change-stage selector))))

(defun.ps+ add-selector-entity ()
  (let ((selector (make-ecs-entity))
        (selector-comp (init-stage-selector)))
    (add-entity-tag selector :selector)
    (add-ecs-component-list
     selector
     (make-point-2d :x (get-menu-param :selector :x)
                    :y (get-menu-param :selector :y))
     selector-comp
     (make-script-2d :func #'process-selector))
    (add-ecs-entity selector)
    (let ((cursor (make-selector-cursor-entity selector)))
      (add-on-change-stage (lambda (index)
                             (set-selector-cursor-position cursor index))
                           selector-comp)
      (add-ecs-entity cursor selector))
    (let ((stage-list (get-stage-kind-list)))
      (dotimes (i (length stage-list))
        (add-ecs-entity
         (make-selector-item
          :stage-name (nth i stage-list)
          :index i)
         selector)))))

;; - item - ;;

(defun.ps+ get-left-center-of-selector-item (index)
  "Note: Return local point in selector's coordinate"
  (make-point-2d :x #lx0
                 :y (* -1 (get-menu-param :selector :item :height) index)))

(defun.ps+ make-selector-item (&key stage-name index)
  (let ((item (make-ecs-entity)))
    (frame-promise-then
     (make-text-model-promise stage-name
                              :size (get-menu-param :selector :item :text-size)
                              :color #xffffff)
     (lambda (model)
       (let ((pnt (get-left-center-of-selector-item index)))
         (with-slots (y) pnt
           ;; Note: Currently, all characters are lower,
           ;; so decreasing half of height is excess.
           (setf y (- y (* 1/4 (get-menu-param :selector :item :height)))))
         (add-ecs-component-list
          item
          pnt
          (make-model-2d :model model
                         :depth 10)))))
    item))

;; - cursor - ;;

(defun.ps+ set-selector-cursor-position (cursor index)
  (with-ecs-components (point-2d) cursor
    (copy-point-2d-to point-2d
                      (get-left-center-of-selector-item index))
    (with-slots (x) point-2d
      (setf x (- x (get-entity-param cursor :width)
                 (get-menu-param :selector :cursor :offset-x))))))

(defun.ps+ make-selector-cursor-entity (selector)
  (check-entity-tags selector :selector)
  (let ((cursor (make-ecs-entity))
        (width (get-menu-param :selector :cursor :width))
        (height (get-menu-param :selector :cursor :height)))
    (add-entity-tag cursor :selector-arrow)
    (add-ecs-component-list
     cursor
     (make-point-2d)
     (make-model-2d :model (make-wired-polygon
                            :pnt-list (list (list 0 (* 1/2 height))
                                            (list 0 (* -1/2 height))
                                            (list width 0))
                            :color #xffffff)
                    :depth 0)
     (init-entity-params :width width))
    (set-selector-cursor-position cursor 0)
    cursor))

;; --- logo --- ;;

(defun.ps+ add-logo ()
  (frame-promise-then
   (make-texture-model-promise :width (get-menu-param :logo :width)
                               :texture-name "logo")
   (lambda (model)
     (let ((logo (make-ecs-entity)))
       (add-ecs-component-list
        logo
        (make-point-2d :x (get-menu-param :logo :x)
                       :y (get-menu-param :logo :y))
        (make-model-2d :model model))
       (add-ecs-entity logo)))))

;; --- controller --- ;;

(defun.ps+ start-game-p ()
  (or (key-down-now-p :a)
      (eq (get-left-mouse-state) :down-now)
      (eq (get-total-touch-state) :down-now)))

;; --- state --- ;;

(def-game-state menu ((parent (make-ecs-entity)))
  :start-process
  (state-lambda (parent)
    (with-ecs-entity-parent (parent)
      (let ((area (make-text-area :font-size (get-menu-param
                                              :press-key-info :text-size)
                                  :text-align :left)))
        (add-text-to-area area
                          :text "Press z key to start ..."
                          :color #xffffff)
        (add-ecs-component-list
         area
         (make-point-2d :x (get-menu-param :press-key-info :x)
                        :y (get-menu-param :press-key-info :y)))
        (add-ecs-entity area)
        (add-selector-entity))
      (add-logo))
    t)
  :process
  (state-lambda ()
    (when (start-game-p)
      (let* ((selector-entity (find-a-entity-by-tag :selector))
             (selector (get-ecs-component 'stage-selector selector-entity)))
        (make-state :main
                    :stage-kind (get-stage-kind selector)))))
  :end-process
  (state-lambda (parent)
    (register-next-frame-func
     (lambda () (delete-ecs-entity parent)))
    t))
