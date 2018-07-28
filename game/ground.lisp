(defpackage clw-shinobi-hashiri/game/ground
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-ground
           :get-ground-height
           :add-on-ground-scroll)
  (:import-from :clw-shinobi-hashiri/game/parameter
                :get-param
                :get-depth))
(in-package :clw-shinobi-hashiri/game/ground)

;; TODO: Prevent overflow by scroll

;; --- event --- ;;

(defun.ps+ add-on-ground-scroll (entity func &optional (ground (find-ground)))
  "Add scroll event function. The func takes entity and scroll speed as arguments.
If the entity is deleted, the func is also deleted"
  (setf (gethash (ecs-entity-id entity) (get-entity-param ground :on-scroll))
        func))

;; --- --- ;;

(defvar.ps+ *next-wall-id* 0)

(defstruct.ps+ (wall (:include ecs-component))
    id height width name)

(defvar.ps+ *dummy-scroll-speed* 1.5)

(defun.ps+ find-ground ()
  (find-a-entity-by-tag :ground))

(defun.ps+ get-highest-wall (center-x width &optional (ground (find-ground)))
  (check-entity-tags ground :ground)
  (let ((min-x (- center-x (* 1/2 width)))
        (max-x (+ center-x (* 1/2 width)))
        (left (point-2d-x (get-ecs-component 'point-2d ground)))
        (max-height #ly-1000)
        (result nil))
    (dolist (wall (get-entity-param ground :stage))
      (let ((height (wall-height wall))
            (right (+ left (wall-width wall))))
        (when (> left max-x)
          (return-from get-highest-wall result))
        (when (and (> right min-x)
                   (>= height 0)
                   (> height max-height))
          (setf max-height height)
          (setf result wall))
        (setf left right)))
    result))

(defun.ps+ get-ground-height (center-x width &optional (ground (find-ground)))
  (let ((wall (get-highest-wall center-x width ground)))
    (if wall
        (wall-height wall)
        #ly-1000)))

(defun.ps+ scroll-ground (ground)
  (check-entity-tags ground :ground)
  (let ((scroll-speed *dummy-scroll-speed*)
        (on-scroll-hash (get-entity-param ground :on-scroll)))
    (with-ecs-components (point-2d) ground
      (incf (point-2d-x point-2d)
            (* -1 scroll-speed)))
    (maphash (lambda (entity-id func)
               (let ((entity (find-a-entity (lambda (target)
                                              (= (ecs-entity-id target))
                                              entity-id))))
                 (if (find-the-entity entity)
                     (funcall func entity scroll-speed)
                     (register-next-frame-func
                      (lambda () (remhash entity on-scroll-hash))))))
             on-scroll-hash)))

;; Definition of stage
;; ((height distance) (height distance) ...)
(defmacro.ps+ create-stage (&rest def)
  `(list ,@(mapcar (lambda (block-def)
                     (destructuring-bind (height width &key name)
                         block-def
                       `(make-wall :id (incf *next-wall-id*)
                                   :height ,height
                                   :width ,width
                                   :name ,name)))
                   def)))

(defun.ps+ interpret-stage (ground stage)
  ;; TODO: Incrementaly add (and delete) models
  (let ((x 0))
    (dolist (wall stage)
      (with-slots (width height) wall
        (add-ecs-component
         (make-model-2d :model (make-solid-rect :width width
                                                :height height
                                                :color #x000000)
                        :depth (get-depth :ground)
                        :offset (make-point-2d :x x))
         ground)
        (incf x width))))
  (set-entity-param ground :stage stage))

(defun.ps+ init-ground (parent)
  (let ((ground (make-ecs-entity)))
    (add-entity-tag ground :ground)
    (add-ecs-component-list
     ground
     (make-point-2d)
     (make-script-2d :func (lambda (entity)
                             (scroll-ground entity)))
     (init-entity-params :on-scroll (make-hash-table)))
    (interpret-stage
     ground
     (create-stage (#ly50 #lx300)
                   (#ly-1 #lx80)
                   (#ly80 #lx100)
                   (#ly200 #lx 200)))
    (add-ecs-entity ground parent)
    ground))
