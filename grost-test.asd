(in-package :cl-user)
(defpackage grost-test-asd
  (:use :cl :asdf))
(in-package :grost-test-asd)

(defsystem grost-test
  :author "yaz"
  :license ""
  :depends-on (:grost
               :prove)
  :components ((:module "t"
                :components
                ((:file "grost"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
