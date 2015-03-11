;;; -*- mode: common-lisp -*-
(in-package :cl-user)
(defpackage grost.db
  (:use :cl)
  (:import-from :grost.config
                :config)
  (:import-from :datafly
                :*connection*
                :connect-cached
		:retrieve-all)
  (:import-from :sxql
                :select
		:insert-into
		:update
		:*
		:from
                :where
		:=
		:set=
		:limit)
  (:import-from :integral
                :table-definition
		:ensure-table-exists
		:execute-sql
		:connect-toplevel
		:<dao-table-class>
                :save-dao
		:select-dao
                :create-dao
		:find-dao
                :delete-dao
		:migrate-table)
  (:export :connection-settings
           :db
           :with-connection))
(in-package :grost.db)

;; (setf integral:*auto-migration-mode* t)

;;; 準備
;;; CREATE USER grostuser IDENTIFIED BY 'password';
;;; CREATE SCHEMA IF NOT EXISTS `grostdb` DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;
;;; GRANT ALL PRIVILEGES ON grostdb.* TO 'grostuser'@'localhost' IDENTIFIED BY 'password';
;;; GRANT ALL PRIVILEGES ON grostdb.* TO 'grostuser'@'127.0.0.1' IDENTIFIED BY 'password';
;;; (:grostdb :mysql :database-name "grostdb" :host "127.0.0.1" :username "grostuser" :passwword "password")

(defclass session-table ()
  ((sessionid :type (varchar 180)
	      :primary-key t
	      :initarg :sessionid
	      :accessor sessionid)
   (session_data :type longtext
		 :initarg :session_data
		 :accessor session_data))
  (:metaclass <dao-table-class>))

(defclass user-table ()
  ((userno :type integer
	   :primary-key t
	   :initarg :userno
	   :accessor userno)
   (userid :type (varchar 180)
	   :initarg :userid
	   :accessor userid)
   (name :type (varchar 180)
	 :initarg name
	 :accessor :name)
   (password :type (varchar 2048)
	     :initarg :password
	     :accessor password)
   (passwordhint :type longtext
		 :initarg :password-hint
		 :accessor passwordhint)
   (status :type (varchar 180)
	   :initform "Normal"
	   :initarg :status
	   :accessor status)
   (division :type (varchar 180)
	     :initform "Guest"
	     :initarg :division
	     :accessor division)
   (section :type (varchar 180)
	    :initform "Guest"
	    :initarg :section
	    :accessor section)
   (frag :type (varchar 180)
	 :initarg :frag
	 :accessor frag)
   (mail :type (varchar 180)
	 :initarg :mail
	 :accessor mail)
   (phone :type (varchar 180)
	  :initarg :phone
	  :accessor phone))
  (:metaclass <dao-table-class>))

(connect-toplevel :mysql
		  :database-name "grostdb"
		  :username "grostuser"
		  :host "127.0.0.1"
		  :password "password")






(defun connection-settings (&optional (db :maindb))
  (cdr (assoc db (config :databases))))

(defun db (&optional (db :maindb))
  (apply #'connect-cached (connection-settings db)))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))
