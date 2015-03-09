(in-package :cl-user)
(defpackage grost-asd
  (:use :cl :asdf))
(in-package :grost-asd)

(defsystem grost
  :version "0.1"
  :author "yaz"
  :license ""
  :depends-on (:clack
               :caveman2
               :envy
               :cl-ppcre

               ;; for @route annotation
               :cl-syntax-annot

               ;; HTML Template
               :djula

               ;; for DB
               :datafly
               :sxql)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (load-op grost-test))))
