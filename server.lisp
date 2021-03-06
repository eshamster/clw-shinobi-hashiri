(defpackage clw-shinobi-hashiri/server
  (:use :cl
        :cl-web-2d-game
        :cl-markup)
  (:export :start
           :stop)
  (:import-from :clw-shinobi-hashiri/game/game
                :init-func
                :update-func))
(in-package :clw-shinobi-hashiri/server)

;; --- Definitions about directories --- ;;

(defvar *script-dir*
  (merge-pathnames "static/"
                   (asdf:component-pathname
                    (asdf:find-system :clw-shinobi-hashiri))))

(defvar *js-relative-dir* "js/")

(defvar *downloaded-js-dir*
  (merge-pathnames *js-relative-dir* *script-dir*))

;; --- Make js main file --- ;;

(defvar *js-game-file*
  (merge-pathnames "game.js" *downloaded-js-dir*))

(defun make-js-main-file ()
  (with-open-file (out *js-game-file*
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (princ
     (pse:with-use-ps-pack (:this)
       (let ((width 800)
             (height 600))
         (start-2d-game :screen-width width
                        :screen-height height
                        :resize-to-screen-p t
                        :camera (init-camera 0 0 width height)
                        :rendered-dom (document.query-selector "#renderer")
                        :stats-dom (document.query-selector "#stats-output")
                        :monitoring-log-dom (document.query-selector "#monitor")
                        :event-log-dom (document.query-selector "#eventlog")
                        :init-function init-func
                        :update-function update-func)))
     out)))

;; --- Server --- ;;

(defvar *app* (make-instance 'ningle:<app>))

(defvar *server* nil)

(setf (ningle:route *app* "/" :method :GET)
      (lambda (params)
        (declare (ignorable params))
        (make-js-main-file)
        (with-output-to-string (str)
          (let ((cl-markup:*output-stream* str))
            (html5 (:head
                    (:title "clw-shinobi-hashiri")
                    (:link :rel "stylesheet" :type "text/css" :href "css/viewer.css" nil)
                    (dolist (js-src (make-src-list-for-script-tag *js-relative-dir*))
                      (markup (:script :src js-src nil))))
                   (:body
                    (:div :id "stats-output")
                    (:div :id "renderer" nil)
                    (dolist (id '("monitor" "eventlog"))
                      (markup
                       (:div
                        :class "log-box"
                        (:label :class "open-close-label"
                                :id (format nil "open-close-label-~A" id)
                                :for (format nil "open-close-~A" id) nil)
                        (:input :class "open-close"
                                :id (format nil "open-close-~A" id)
                                :type "checkbox")
                        (:div :class "log-panel" :id id "(Log Area)"))))
                    (:script :src "js/game.js" nil)))))))

(defun stop ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)))

(defun start (&key (port 5000) (address "0.0.0.0"))
  (ensure-js-files *downloaded-js-dir*)
  (stop)
  (setf *server*
        (clack:clackup
         (lack:builder
          (:static :path (lambda (path)
                           (print path)
                           (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)"
                                           path)
                               path
                               nil))
                   :root *script-dir*)
          *app*)
         :port port
         :address address)))
