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
           :get-scroll-speed)
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


;; --- stage structure --- ;;

(defstruct.ps+ (wall (:include ecs-component))
    id height width name)

(defun.ps+ clone-wall (wall)
  (with-slots (id height width name) wall
    (make-wall :id id :height height :width width :name name)))

(defstruct.ps+ stage-info
    (fn-get-wall (lambda (id info) (declare (ignore id info)))))

(defstruct.ps+
    (loop-stage-info
       (:include stage-info
                 (fn-get-wall
                  (lambda (id info)
                    (let ((lst (loop-stage-info-wall-list info)))
                      (nth (mod id (length lst)) lst))))))
    wall-list)

(defstruct.ps+ stage-manager
    stage-info)

;; --- stage manager process --- ;;

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
                                          0)
                                   (register-next-frame-func
                                    (lambda () (delete-ecs-entity entity)))))))))
    entity))

(defun.ps+ generate-required-walls (manager ground &optional latest-wall)
  (check-type manager stage-manager)
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
      (let* ((stage-info (stage-manager-stage-info manager))
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
            (generate-required-walls manager ground new-wall-entity)))))))

;; --- random stage generator --- ;;

(defvar.ps+ *random-generator-params*
    (convert-to-layered-hash
     (:height (:min #ly50 :max #ly800
               :diff (:min #ly50 :max #ly500))
      :width (:min #lx30 :max #lx400)
      :hole (:ratio 0.3 :min #lx80 :max #lx300))))

(defun.ps-only random1 () (random))
(defun random1 () (random 1.0))

(defmacro.ps+ get-param-randomly (params key)
  (flet ((get-my-param (key1 key2)
           `(get-layered-hash ,params ,key1 ,key2)))
    `(lerp-scalar ,(get-my-param key :min)
                  ,(get-my-param key :max)
                  (random1))))

(defun.ps+ wall-is-hole-p (wall-cmp)
  (< (wall-height wall-cmp) 0))

(defun.ps+ calc-next-height (params pre-wall)
  (let ((pre-height (wall-height pre-wall)))
    (labels ((rec ()
               (let ((height (get-param-randomly params :height)))
                 (if (and (> (abs (- pre-height height))
                               (get-layered-hash params :height :diff :min))
                            ;; Intentionally omit "abs"
                            (< (- height pre-height)
                               (get-layered-hash params :height :diff :max)))
                     (return-from rec height)
                     (rec)))))
      (rec))))

(defun.ps+ get-wall-randomly (id info)
  (declare (ignore id))
  (check-type info random-stage-info)
  (with-slots (pre-wall (params random-params) pre-is-hole-p) info
    (let ((result
           (cond ((null pre-wall)
                  (make-wall :height #ly50 :width #lx500))
                 ((and (not pre-is-hole-p)
                       (> (get-layered-hash params :hole :ratio)
                          (random1)))
                  (make-wall :height -1
                             :width (get-param-randomly params :hole)))
                 (t (make-wall :height (calc-next-height params pre-wall)
                               :width (get-param-randomly params :width))))))
      (if (wall-is-hole-p result)
          (setf pre-is-hole-p t)
          (progn (setf pre-wall result
                       pre-is-hole-p nil)))
      result)))

(defstruct.ps+
    (random-stage-info
     (:include stage-info
               (fn-get-wall #'get-wall-randomly)))
    pre-wall
  (pre-is-hole-p nil)
  random-params)

(defun.ps+ init-random-stage-info (&optional (params *random-generator-params*))
  (make-random-stage-info :random-params params))

;; --- --- ;;

(defvar.ps+ *dummy-scroll-speed* 2)

(defun.ps+ find-ground ()
  (find-a-entity-by-tag :ground))

(defun.ps+ get-scroll-speed (&optional (ground (find-ground)))
  (declare (ignore ground))
  *dummy-scroll-speed*)

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

;; Definition of stage
;; ((height distance [:name "name"]) (height distance [:name "name"]) ...)
(defmacro.ps+ create-stage (&rest def)
  `(list ,@(mapcar (lambda (block-def)
                     (destructuring-bind (height width &key name)
                         block-def
                       `(make-wall :height ,height
                                   :width ,width
                                   :name ,name)))
                   def)))

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

;; for test
(defvar.ps+ *static-stage-info*
    (make-loop-stage-info
     :wall-list (create-stage (#ly50 #lx300)
                              (#ly-1 #lx80)
                              (#ly80 #lx100)
                              (#ly350 #lx200)
                              (#ly700 #lx200)
                              (#ly-1 #lx300)
                              (#ly100 #lx200))))

(defun.ps+ init-ground (parent)
  (let* ((ground (make-ecs-entity))
         (stage-manager
          (make-stage-manager
           ;; :stage-info *static-stage-info*
           :stage-info (init-random-stage-info)
           )))
    (add-entity-tag ground :ground)
    (add-ecs-component-list
     ground
     (make-point-2d)
     (make-script-2d :func (lambda (entity)
                             (generate-required-walls stage-manager entity)
                             (scroll-ground entity)
                             (debug-ground entity)))
     (init-entity-params :on-scroll (make-hash-table)))
    (add-ecs-entity ground parent)
    ground))
