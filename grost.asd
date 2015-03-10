(in-package :cl-user)
(defpackage grost-asd
  (:use :cl :asdf))
(in-package :grost-asd)

(defsystem grost
  :version "0.1"
  :author "yaz"
  :license "LLGPL"
  :depends-on (:clack
               :caveman2
               :envy
               :cl-ppcre
	       :cl-interpol

               ;; for @route annotation
               :cl-syntax-annot

               ;; HTML Template
               :djula

               ;; for DB
               :datafly
               :sxql
	       :integral

	       ;; server
	       :hunchentoot
	       :woo

	       ;; util
	       :ironclad
	       :local-time
	       :mt19937)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db" "util"))
                 (:file "web" :depends-on ("view" "util"))
                 (:file "view" :depends-on ("config" "util"))
                 (:file "db" :depends-on ("config" "util"))
                 (:file "config" :depends-on ("util"))
		 (:file "util"))))
  :description ""
  :in-order-to ((test-op (load-op grost-test))))
