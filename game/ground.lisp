(defpackage clw-shinobi-hashiri/game/ground
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-ground
           :get-ground-height
           :add-on-ground-scroll
           :get-wall-info
           :get-highest-wall-info
           :get-scroll-speed
           :set-scroll-speed-scale
           :get-stage-kind-list)
  (:import-from :clw-shinobi-hashiri/game/parameter
                :get-param
                :get-depth)
  (:import-from :clw-shinobi-hashiri/game/stage/utils
                :wall
                :wall-id
                :wall-width
                :wall-height
                :clone-wall
                :stage-info-fn-get-wall
                :stage-info-fn-get-scroll-speed
                :stage-info-fn-process)
  (:import-from :clw-shinobi-hashiri/game/stage/regular
                :init-random-stage-info)
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

;; --- stage manager --- ;;

(defstruct.ps+ stage-manager
    stage-info
  (scroll-speed-scale 1))

(defun.ps+ get-max-id-wall (ground)
  (let (max-id result)
    (do-wall-entity (entity ground)
      (let ((id (wall-id (get-ecs-component 'wall entity))))
        (when (or (null max-id)
                  (< max-id id))
          (setf max-id id
                result entity))))
    result))

(defun.ps+ get-right-of-wall (wall-entity)
  (check-entity-tags wall-entity :wall)
  (+ (point-2d-x (calc-global-point wall-entity))
     (wall-width (get-ecs-component 'wall wall-entity))))

(defun.ps+ make-next-wall-entity (&key wall-cmp id local-left)
  (let ((entity (make-ecs-entity))
        (cmp (clone-wall wall-cmp)))
    (add-entity-tag entity :wall)
    (setf (wall-id cmp) id)
    (with-slots (width height) wall-cmp
      (add-ecs-component-list
       entity
       cmp
       (make-point-2d :x local-left)
       (make-model-2d :model (make-solid-rect :width width
                                              :height height
                                              :color #x000000)
                      :depth (get-depth :ground))
       (make-script-2d :func (lambda (entity)
                               (let ((wall (get-ecs-component 'wall entity))
                                     (pnt (calc-global-point entity)))
                                 (when (< (+ (point-2d-x pnt) (wall-width wall))
                                          #ly-500)
                                   (register-next-frame-func
                                    (lambda () (delete-ecs-entity entity)))))))))
    entity))

(defun.ps+ generate-required-walls (ground &optional latest-wall)
  (check-entity-tags ground :ground)
  (when latest-wall
    (check-entity-tags latest-wall :wall))
  (let* ((rightest-wall (if latest-wall latest-wall (get-max-id-wall ground)))
         (params (if rightest-wall
                     (list :right (get-right-of-wall rightest-wall)
                           :next-id (1+ (wall-id (get-ecs-component 'wall rightest-wall))))
                     (list :right 0
                           :next-id 0))))
    (when (< (getf params :right)
             (get-param :field :width))
      (let* ((manager (get-entity-param ground :stage-manager))
             (stage-info (stage-manager-stage-info manager))
             (get-wall (stage-info-fn-get-wall stage-info))
             (next-id (getf params :next-id))
             (next-wall-cmp (funcall get-wall next-id stage-info)))
        (when next-wall-cmp
          (let ((new-wall-entity (make-next-wall-entity
                                  :wall-cmp next-wall-cmp
                                  :id next-id
                                  :local-left (- (getf params :right)
                                                 (point-2d-x (get-ecs-component
                                                              'point-2d ground))))))
            (add-ecs-entity-to-buffer new-wall-entity ground)
            (generate-required-walls ground new-wall-entity)))))))

;; --- gound --- ;;

(defun.ps+ find-ground ()
  (find-a-entity-by-tag :ground))

(defun.ps+ get-stage-manager (ground)
  (check-entity-tags ground :ground)
  (get-entity-param ground :stage-manager))

(defun.ps+ get-scroll-speed (&optional (ground (find-ground)))
  (let* ((manager (get-stage-manager ground))
         (info (stage-manager-stage-info manager)))
    (* (funcall (stage-info-fn-get-scroll-speed info) info)
       (stage-manager-scroll-speed-scale manager))))

(defun.ps+ set-scroll-speed-scale (value &optional (ground (find-ground)))
  (let ((manager (get-stage-manager ground)))
    (setf (stage-manager-scroll-speed-scale manager)
          value)))

(defun.ps+ get-wall-info (wall-entity)
  (let ((wall-cmp (get-ecs-component 'wall wall-entity)))
    (list :height (wall-height wall-cmp)
          :width (wall-width wall-cmp)
          :entity wall-entity)))

(defun.ps+ get-highest-wall-info (center-x width &optional (ground (find-ground)))
  (check-entity-tags ground :ground)
  (let ((min-x (- center-x (* 1/2 width)))
        (max-x (+ center-x (* 1/2 width)))
        (max-height #ly-1000)
        (highest-entity nil))
    (do-wall-entity (wall-entity ground)
      (let* ((wall-cmp (get-ecs-component 'wall wall-entity))
             (height (wall-height wall-cmp))
             (left (point-2d-x (calc-global-point wall-entity)))
             (right (+ left (wall-width wall-cmp))))
        (when (and (< left max-x)
                   (> right min-x)
                   (>= height 0)
                   (> height max-height))
          (setf max-height height)
          (setf highest-entity wall-entity))))
    (when highest-entity
      (get-wall-info highest-entity))))

(defun.ps+ get-ground-height (center-x width &optional (ground (find-ground)))
  (let ((wall (get-highest-wall-info center-x width ground)))
    (if wall
        (getf wall :height)
        #ly-1000)))

(defun parse-int (id)
  ;; This is a dummy function.
  id)

(defun.ps+ scroll-ground (ground)
  (check-entity-tags ground :ground)
  (let ((scroll-speed (get-scroll-speed ground))
        (on-scroll-hash (get-entity-param ground :on-scroll)))
    (with-ecs-components (point-2d) ground
      (incf (point-2d-x point-2d)
            (* -1 scroll-speed)))
    (maphash (lambda (entity-id func)
               ;; Note: In JavaSctipt, Object.keys(<hash>) returns array of string,
               ;; even if each of keys is number.
               (let ((entity (find-a-entity (lambda (target)
                                              (= (ecs-entity-id target)
                                                 (parse-int entity-id))))))
                 (if (find-the-entity entity)
                     (funcall func entity scroll-speed)
                     (register-next-frame-func
                      (lambda () (remhash entity on-scroll-hash))))))
             on-scroll-hash)))

(defun.ps+ process-stage-info (ground)
  (check-entity-tags ground :ground)
  (let* ((manager (get-entity-param ground :stage-manager))
         (info (stage-manager-stage-info manager)))
    (funcall (stage-info-fn-process info) info)))

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

(defun.ps+ get-stage-kind-list ()
  (list :regular :needle :big-hole))

(defun.ps+ init-ground (stage-kind)
  (let* ((ground (make-ecs-entity))
         (stage-manager
          (make-stage-manager
           :stage-info (init-random-stage-info stage-kind))))
    (add-entity-tag ground :ground)
    (add-ecs-component-list
     ground
     (make-point-2d)
     (make-script-2d :func (lambda (entity)
                             (generate-required-walls entity)
                             (scroll-ground entity)
                             (process-stage-info entity)
                             (debug-ground entity)))
     (init-entity-params :on-scroll (make-hash-table)
                         :stage-manager stage-manager))
    (add-ecs-entity ground)
    ground))
