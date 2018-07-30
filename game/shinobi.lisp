(defpackage clw-shinobi-hashiri/game/shinobi
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-shinobi)
  (:import-from :clw-shinobi-hashiri/game/gravity
                :gravity
                :make-gravity
                :gravity-decrease-rate
                :start-gravity
                :stop-gravity)
  (:import-from :clw-shinobi-hashiri/game/ground
                :get-ground-height
                :get-wall-info
                :get-highest-wall-info
                :add-on-ground-scroll)
  (:import-from :clw-shinobi-hashiri/game/parameter
                :get-param
                :get-depth))
(in-package :clw-shinobi-hashiri/game/shinobi)

;; --- utils --- ;;

(defun.ps+ find-shinobi ()
  (find-a-entity-by-tag :shinobi))

(defun.ps+ get-my-ground-info (func shinobi &optional (extra-dist 0))
  (check-entity-tags shinobi :shinobi)
  (funcall func
           (+ (point-2d-x (get-ecs-component 'point-2d shinobi))
              (* 1/2 extra-dist))
           (+ (get-entity-param shinobi :width)
              extra-dist)))

(defun.ps+ get-my-ground-height (shinobi &optional (extra-dist 0))
  (get-my-ground-info #'get-ground-height shinobi extra-dist))

(defun.ps+ get-my-highest-wall-info (shinobi &optional (extra-dist 0))
  (get-my-ground-info #'get-highest-wall-info shinobi extra-dist))

;; --- jump --- ;;

;; TODO: Split controller

(defvar.ps+ *jump-key* :a)

(defstruct.ps+ (shinobi-state (:include game-state)) shinobi)

(defstruct.ps+
    (jumping-state
     (:include shinobi-state
               (start-process (lambda (state)
                                (let ((shinobi (shinobi-state-shinobi state)))
                                  (set-entity-param shinobi :on-ground-p nil))
                                t))
               (process (lambda (state)
                          (let ((shinobi (shinobi-state-shinobi state)))
                            (set-entity-param shinobi :on-ground-p nil)
                            (setf (speed-2d-y (get-ecs-component 'speed-2d shinobi))
                                  (get-param :shinobi :jump :speed))
                            (symbol-macrolet ((duration (jumping-state-duration state)))
                              (decf duration)
                              (cond ((is-key-up-now *jump-key*)
                                     (make-falling-state :shinobi shinobi))
                                    ((<= duration 0)
                                     (make-gliding-state :shinobi shinobi)))))))))
  (duration (get-param :shinobi :jump :max-time)))

(defun.ps+ land-immediately (shinobi)
  (let* ((center (get-ecs-component 'point-2d shinobi))
         (ground-height (get-my-ground-height shinobi)))
    (setf (point-2d-y center)
          (+ ground-height (* 1/2 (get-entity-param shinobi :height))))
    (setf (speed-2d-y (get-ecs-component 'speed-2d shinobi))
          0)))

(defun.ps+ process-in-falling-process (state)
  (let ((shinobi (shinobi-state-shinobi state)))
    (cond ((get-entity-param shinobi :on-ground-p)
           (make-on-ground-state :shinobi shinobi))
          ((is-key-down-now *jump-key*)
           (if (>= (get-my-ground-height shinobi) 0)
               (progn
                 (land-immediately shinobi)
                 (set-entity-param shinobi :on-ground-p t)
                 (make-on-ground-state :shinobi shinobi))
               (progn
                 (make-gliding-state :shinobi shinobi)))))))

(defstruct.ps+
    (falling-state
     (:include shinobi-state
               (process #'process-in-falling-process))))

(defun.ps+ process-in-gliding (state)
  (let ((shinobi (shinobi-state-shinobi state)))
    (with-ecs-components (speed-2d) shinobi
      (symbol-macrolet ((y (speed-2d-y speed-2d)))
        (let ((max-speed (* -1 (get-param :shinobi :glide :max-fall-speed))))
          (when (< y max-speed)
            (setf y max-speed)))))
    (cond ((get-entity-param shinobi :on-ground-p)
           (make-on-ground-state :shinobi shinobi))
          ((is-key-up-now *jump-key*)
           (make-falling-state :shinobi shinobi)))))

(defun.ps+ setf-gravity-rate (state rate)
  (check-type state shinobi-state)
  (let ((shinobi (shinobi-state-shinobi state)))
    (setf (gravity-decrease-rate (get-ecs-component 'gravity shinobi))
          rate)))

;; TODO: Decrease speed (go back) according to scroll speed
(defstruct.ps+
    (gliding-state
     (:include shinobi-state
               (start-process (lambda (state)
                                (setf-gravity-rate state
                                                   (get-param :shinobi :glide :gravity-rate))
                                (let* ((shinobi (shinobi-state-shinobi state))
                                       (speed (get-ecs-component 'speed-2d shinobi)))
                                  (setf (speed-2d-x speed)
                                        (* -1 (get-param :shinobi :glide :back-speed)))
                                  (unless (get-entity-param shinobi :has-glided-p)
                                    (set-entity-param shinobi :has-glided-p t)
                                    (setf (speed-2d-y speed)
                                          (get-param :shinobi :glide :first-y-speed))))
                                t))
               (process #'process-in-gliding)
               (end-process (lambda (state)
                              (let* ((shinobi (shinobi-state-shinobi state))
                                     (speed (get-ecs-component 'speed-2d shinobi)))
                                (setf (speed-2d-x speed) 0))
                              (setf-gravity-rate state 1)
                              t)))))

(defstruct.ps+
    (on-ground-state
     (:include shinobi-state
               (start-process (lambda (state)
                                (let ((shinobi (shinobi-state-shinobi state)))
                                  (set-entity-param shinobi :has-glided-p nil))
                                t))
               (process (lambda (state)
                          (let* ((shinobi (shinobi-state-shinobi state))
                                 (default-x (get-param :shinobi :on-ground :default-x))
                                 (speed (get-param :shinobi :on-ground :return-speed))
                                 (point (get-ecs-component 'point-2d shinobi)))
                            (symbol-macrolet ((x (point-2d-x point)))
                              (when (< x default-x)
                                (setf x (min default-x (+ x speed)))))
                            (cond ((is-key-down-now *jump-key*)
                                   (make-jumping-state :shinobi shinobi))
                                  ((> (get-bottom shinobi) (get-my-ground-height shinobi))
                                   (set-entity-param shinobi :on-ground-p nil)
                                   (make-falling-state :shinobi shinobi)))))))))

;; - climb states - ;;

(defstruct.ps+ (climb-state (:include shinobi-state))
    target-wall)

(defstruct.ps+
    (holding-wall-state
     (:include climb-state
               (start-process (lambda (state)
                                (with-slots (shinobi target-wall) state
                                  (set-entity-param shinobi :scroll-p t)
                                  (stop-gravity shinobi)
                                  (with-ecs-components (speed-2d point-2d) shinobi
                                    (setf (speed-2d-y speed-2d) 0)
                                    (when (find-the-entity target-wall)
                                      (symbol-macrolet ((x (point-2d-x point-2d)))
                                        (setf x (- (point-2d-x
                                                    (calc-global-point target-wall))
                                                   (* 1/2 (get-entity-param shinobi :width))))))))
                                t))
               (process (lambda (state)
                          (when (is-key-down-now *jump-key*)
                            (with-slots (shinobi target-wall) state
                              (make-climb-jumping-state :shinobi shinobi
                                                        :target-wall target-wall)))))
               (end-process (lambda (state)
                              (let ((shinobi (shinobi-state-shinobi state)))
                                (when (not (typep (get-next-state shinobi) 'climb-state))
                                  (set-entity-param shinobi :scroll-p nil)
                                  (start-gravity shinobi)))
                              t)))))

(defstruct.ps+
    (climb-jumping-state
     (:include climb-state
               (start-process (lambda (state)
                                (let ((shinobi (shinobi-state-shinobi state)))
                                  (set-entity-param shinobi :scroll-p t)
                                  (start-gravity shinobi))
                                t))
               (process (lambda (state)
                          ;; XXX: Search length should be decided according to tolerance
                          (let ((shinobi (shinobi-state-shinobi state))
                                (duration (get-param :shinobi :climb-jump :duration))
                                (min-speed (get-param :shinobi :climb-jump :min-speed))
                                (max-speed (get-param :shinobi :climb-jump :max-speed)))
                            (symbol-macrolet ((time (climb-jumping-state-time state)))
                              (when (< time duration)
                                (setf (speed-2d-y (get-ecs-component 'speed-2d shinobi))
                                      (lerp-scalar min-speed max-speed (/ time duration))))
                              (incf time))
                            (when (is-key-up-now *jump-key*)
                              (with-slots (target-wall) state
                                (make-holding-wall-state :shinobi shinobi
                                                         :target-wall target-wall))))))
               (end-process (lambda (state)
                              (let ((shinobi (shinobi-state-shinobi state)))
                                (when (not (typep (get-next-state shinobi) 'climb-state))
                                  (set-entity-param shinobi :scroll-p nil)))
                              t))))
    (time 0))

;; - state utils - ;;

(defun.ps+ debug-print-state (shinobi)
  (add-to-monitoring-log
   (+ "shinobi jump state: "
      (etypecase (cl-web-2d-game/core/game-state::game-state-manager-current-state
                  (get-entity-param shinobi :jump-state-manager))
        (jumping-state "jumping")
        (falling-state "falling")
        (on-ground-state "on-ground")
        (gliding-state "gliding")
        (holding-wall-state "holding-wall")
        (climb-jumping-state "climb-jumping")))))

(defun.ps+ get-nearest-wall (shinobi max-distance &key (error-if-not-found nil) (tolerance #lx0.01))
  (check-entity-tags shinobi :shinobi)
  (let ((bottom (get-bottom shinobi)))
    (unless (> (get-my-ground-height shinobi max-distance)
               bottom)
      (if error-if-not-found
          (error "Wall is not found in near.")
          (return-from get-nearest-wall)))
    (labels ((rec (current-min-dist current-max-dist)
               (if (< (- current-max-dist current-min-dist) tolerance)
                   (let ((info (get-my-highest-wall-info shinobi current-max-dist)))
                     (list :dist current-min-dist
                           :height (getf info :height)
                           :entity (getf info :entity)))
                   (let ((mid (/ (+ current-min-dist current-max-dist) 2)))
                     (if (> (get-my-ground-height shinobi mid) bottom)
                         (rec current-min-dist mid)
                         (rec mid current-max-dist))))))
      (rec (* -1 (get-entity-param shinobi :width))
           max-distance))))

(defun.ps+ get-next-state (shinobi)
  (check-entity-tags shinobi :shinobi)
  (game-state-manager-next-state (get-entity-param shinobi :jump-state-manager)))

(defun.ps+ get-current-state (shinobi)
  (check-entity-tags shinobi :shinobi)
  (game-state-manager-current-state (get-entity-param shinobi :jump-state-manager)))

(defun.ps+ process-jump-state (shinobi)
  (check-entity-tags shinobi :shinobi)
  (let ((state-manager (get-entity-param shinobi :jump-state-manager))
        (current-state (get-current-state shinobi)))
    ;; Note: Probably, these states change can be implemented more well,
    ;; if there is multiple layer state system...
    (if (not (typep current-state 'climb-state))
        ;; normal states to climb states
        (let ( ;; XXX: Search length should be decided according to scroll speed
              (nearest-wall-info (get-nearest-wall shinobi #lx10)))
          (when (and nearest-wall-info
                     (> (getf nearest-wall-info :height) (get-bottom shinobi))
                     (null (game-state-manager-next-state state-manager)))
            (interrupt-game-state
             (make-holding-wall-state :shinobi shinobi
                                      :target-wall (getf nearest-wall-info :entity))
             state-manager)))
        ;; climb states to normal states
        (let* ((wall-entity (climb-state-target-wall current-state))
               (height (getf (get-wall-info wall-entity) :height)))
          (when (and (< height (get-bottom shinobi))
                     (null (game-state-manager-next-state state-manager)))
            (interrupt-game-state
             (make-falling-state :shinobi shinobi)
             state-manager))))
    (process-game-state state-manager)))

(defun.ps+ get-bottom (shinobi)
  (+ (point-2d-y (get-ecs-component 'point-2d shinobi))
     (* -1/2 (get-entity-param shinobi :height))))

;; --- main --- ;;

(defun.ps+ init-shinobi (parent ground)
  (check-entity-tags ground :ground)
  (let ((shinobi (make-ecs-entity))
        (width (get-param :shinobi :width))
        (height (get-param :shinobi :height)))
    (add-entity-tag shinobi :shinobi)
    (flet ((get-point (entity)
             (get-ecs-component 'point-2d entity)))
      (add-ecs-component-list
       shinobi
       (make-point-2d :x (get-param :shinobi :on-ground :default-x)
                      :y 100)
       (make-gravity :fn-get-width (lambda (entity)
                                     (declare (ignore entity))
                                     width)
                     :fn-get-center-x (lambda (entity)
                                        (point-2d-x (get-point entity)))
                     :fn-get-bottom #'get-bottom
                     :fn-on-ground (lambda (entity)
                                     (set-entity-param entity :on-ground-p t)))
       (make-speed-2d :y 0)
       (make-model-2d :model (make-solid-rect :width width
                                              :height height
                                              :color #x000000)
                      :depth (get-depth :shinobi)
                      :offset (make-point-2d :x (* -1/2 width)
                                             :y (* -1/2 height)))
       (make-script-2d :func (lambda (entity)
                               (process-jump-state entity)
                               (debug-print-state entity)
                               (with-ecs-components (point-2d) entity
                                 (add-to-monitoring-log (+ "x: " (point-2d-x point-2d)))
                                 (add-to-monitoring-log (+ "y: " (point-2d-y point-2d))))))
       (init-entity-params :jump-state-manager
                           (init-game-state-manager (make-falling-state :shinobi shinobi))
                           :jump-input-state :up ;; up-now up down-now down
                           :on-ground-p nil
                           :scroll-p nil
                           :has-glided-p nil
                           :width width
                           :height height)))
    (add-on-ground-scroll
     shinobi
     (lambda (entity scroll-speed)
       (when (get-entity-param entity :scroll-p)
         (symbol-macrolet ((x (point-2d-x (get-ecs-component 'point-2d entity))))
           (setf x (- x scroll-speed)))))
     ground)
    (add-ecs-entity shinobi parent)))
