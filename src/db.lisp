(in-package :cl-user)
(defpackage grost.db
  (:use :cl)
  (:import-from :grost.config
                :config)
  (:import-from :datafly
                :*connection*
                :connect-cached)
  (:export :connection-settings
           :db
           :with-connection))
(in-package :grost.db)

(defun connection-settings (&optional (db :maindb))
  (cdr (assoc db (config :databases))))

(defun db (&optional (db :maindb))
  (apply #'connect-cached (connection-settings db)))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))
