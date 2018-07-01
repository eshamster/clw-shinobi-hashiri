#|
  This file is a part of clw-shinobi-hashiri project.
  Copyright (c) 2018 eshamster (hamgoostar@gmail.com)
|#

#|
  A simple jump game with auto scroll

  Author: eshamster (hamgoostar@gmail.com)
|#

(defsystem clw-shinobi-hashiri
  :version "0.1"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:parenscript
               :ps-experiment
               :cl-ps-ecs
               :cl-web-2d-game
               :ningle
               :cl-markup
               :clack
               :clw-shinobi-hashiri/main)
  :description "A simple jump game with auto scroll"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op clw-shinobi-hashiri/t))))

(defsystem clw-shinobi-hashiri/t
  :class :package-inferred-system
  :depends-on (:ps-experiment
               :ps-experiment/t
               :rove
               :alexandria
               :cl-js
               "clw-shinobi-hashiri/t/main")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
