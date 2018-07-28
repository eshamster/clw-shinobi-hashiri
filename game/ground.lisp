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
                :get-depth)
  (:import-from :alexandria
                :with-gensyms))
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
        (max-height #ly-1000)
        (result nil))
    (do-wall-entity (wall-entity ground)
      (let* ((wall (get-ecs-component 'wall wall-entity))
             (height (wall-height wall))
             (left (point-2d-x (calc-global-point wall-entity)))
             (right (+ left (wall-width wall))))
        (when (and (< left max-x)
                   (> right min-x)
                   (>= height 0)
                   (> height max-height))
          (setf max-height height)
          (setf result wall))))
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
  ;; TODO: Incrementaly add models
  (let ((x 0))
    (dolist (wall-cmp stage)
      (let ((entity (make-ecs-entity)))
        (add-entity-tag entity :wall)
        (with-slots (width height) wall-cmp
          (add-ecs-component-list
           entity
           wall-cmp
           (make-point-2d :x x)
           (make-model-2d :model (make-solid-rect :width width
                                                  :height height
                                                  :color #x000000)
                          :depth (get-depth :ground))
           (make-script-2d :func (lambda (entity)
                                   (let ((wall (get-ecs-component 'wall entity))
                                         (pnt (calc-global-point entity)))
                                     (when (< (+ (point-2d-x pnt) (wall-width wall))
                                              0)
                                       (register-next-frame-func
                                        (lambda () (delete-ecs-entity entity))))))))
          (incf x width))
        (add-ecs-entity entity ground)))))

(defmacro.ps+ do-wall-entity ((var ground) &body body)
  `(progn (check-entity-tags ,ground :ground)
          (do-ecs-entities ,var
            (when (and (has-entity-tag ,var :wall)
                       (eq (ecs-entity-parent ,var) ,ground))
              ,@body))))

(defmacro.ps+ do-wall ((var ground) &body body)
  (with-gensyms (entity)
    `(do-wall-entity (,entity ,ground)
       (let ((,var (get-ecs-component 'wall ,entity)))
         ,@body))))

(defun.ps+ debug-ground (ground)
  (let (max-id min-id)
    (do-wall (wall ground)
      (let ((id (wall-id wall)))
        (when (or (not min-id)
                  (> min-id id))
          (setf min-id id))
        (when (or (not max-id)
                  (< max-id id))
          (setf max-id id))))
    (add-to-monitoring-log
     (+ "Ground: Min ID=" min-id ",Max ID=" max-id))))

(defun.ps+ init-ground (parent)
  (let ((ground (make-ecs-entity)))
    (add-entity-tag ground :ground)
    (add-ecs-component-list
     ground
     (make-point-2d)
     (make-script-2d :func (lambda (entity)
                             (scroll-ground entity)
                             (debug-ground entity)))
     (init-entity-params :on-scroll (make-hash-table)))
    (add-ecs-entity ground parent)
    (interpret-stage
     ground
     (create-stage (#ly50 #lx300)
                   (#ly-1 #lx80)
                   (#ly80 #lx100)
                   (#ly350 #lx200)
                   (#ly700 #lx200)
                   (#ly-1 #lx300)
                   (#ly100 #lx2000)))
    ground))
