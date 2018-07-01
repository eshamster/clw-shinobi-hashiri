(in-package :cl-user)
(defpackage clw-shinobi-hashiri/t/main
  (:use :cl
        :clw-shinobi-hashiri
        :rove))
(in-package :clw-shinobi-hashiri/t/main)

(defvar *port* 21464)

(deftest test-connection
  (unwind-protect
       (progn
         (clw-shinobi-hashiri:start :port *port*)
         (handler-case
             (let ((connected nil))
               (dotimes (i 5)
                 (when (dex:get (format nil "http://localhost:~D" *port*))
                   (setf connected t)
                   (return))
                 (sleep 1))
               (ok connected))
           (error (e)
             (fail (format nil "~A" e)))))
    (clw-shinobi-hashiri:stop)))
