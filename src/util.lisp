;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; coding:utf-8 -*-
;;; Author : yaz <yazw@live.jp>

(in-package :cl-user)
(defpackage grost.util
  (:use :cl))
(in-package :grost.util)
(cl-annot:enable-annot-syntax)

;;; DATA CLEANING
;;; 場当たり的なあれ
@export
(defun numeric-data-cleaning (string)
  (cl-ppcre:regex-replace-all
   "[^(0-9\\.)]"
   (cl-ppcre:regex-replace-all
    "([０-９])"
    (cl-ppcre:regex-replace-all
     "[。．、，,]"
     (cl-ppcre:regex-replace-all "\\.\\.\\." string "")
     ".")
    #'(lambda (match &rest regs)
        (declare (ignore regs))
        (cond
          ((string= "０" match) "0")
          ((string= "１" match) "1")
          ((string= "２" match) "2")
          ((string= "３" match) "3")
          ((string= "４" match) "4")
          ((string= "５" match) "5")
          ((string= "６" match) "6")
          ((string= "７" match) "7")
          ((string= "８" match) "8")
          ((string= "９" match) "9")))
    :simple-calls t)
   ""))

;;; XLSファイルの中の空白についてはJava側で対処した
@export
(defun replace-Z/H-Spaces->single-H-Space (string)
  "任意の数の全角半角の空白にマッチし，１つの半角の空白に置換する。行の末尾であった場合は空白を取り除く。"
  (cl-ppcre:regex-replace
   "( |　)+$"
   (cl-ppcre:regex-replace-all "[ |　]+" string " ")
   ""))

;;; ------------------
;;; xls -> csv -> lisp
;;; ------------------

@export
(defun Java-xls->csv-file-path (input-xls-file-path output-csv-dir)
  (let* ((file-name (cl-ppcre:regex-replace ".xls" (file-namestring input-xls-file-path) ""))
	 (output-csv-file-path (format nil "~A~A.csv" output-csv-dir file-name)))
    (progn #-sbcl
	   (trivial-shell:shell-command
	    (format nil "java -jar ~A ~A > ~A"
	      (merge-pathnames
	       "local-projects/grost/src/Java/xls2csv9.jar"
	       ql:*quicklisp-home*)
	      (format nil "~A" input-xls-file-path)
	      (format nil "~A" output-csv-file-path)))
	   #+sbcl
	   (sb-ext:run-program
	    "java"
	    (list "-jar"
		  (format nil "~A"
			  (merge-pathnames
			   "local-projects/grost/src/Java/xls2csv9.jar"
			   ql:*quicklisp-home*))
		   (format nil "~A" input-xls-file-path))
	     :search t
	     :output output-csv-file-path
	     :if-output-exists :supersede
	     :external-format :utf-8
	     :wait t)
	   (wait-second output-csv-file-path))))

;;; もう要らないかも
(defun wait-second (file &optional (counter 0) (max-count 10) (sleep-second 1))
  (cond ((cl-fad:file-exists-p file)
	 (format nil "~A" file))
	((= counter max-count)
	 (error "wait ~A seconds. file: ~A is not exists" counter file))
	(t (progn
	     (sleep sleep-second)
	     (wait-second file (1+ counter) max-count)))))

@export
(defun list->csv (list output-csv-file-path)
  (with-open-file (out output-csv-file-path
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create
		       :external-format :utf-8)
    (loop :for i :in list
	  :do (format out "~{\"~A\"~^,~}~%" i))))

;;; ----
;;; ISBN
;;; ----
(defun integer-strings->integers-list (string)
  "Xに対応するため最後の要素を除去"
  (mapcar #'parse-integer (nreverse (rest (nreverse (map 'list #'string string))))))

(defun ISBN13->ISBN10-check-digits (list)
  (let ((tmp (- 11 (mod (+ (* 10 (nth 3 list)) (* 9 (nth  4 list)) (* 8 (nth  5 list))
			   (*  7 (nth 6 list)) (* 6 (nth  7 list)) (* 5 (nth  8 list))
			   (*  4 (nth 9 list)) (* 3 (nth 10 list)) (* 2 (nth 11 list))) 11))))
    (if (= tmp 10)
	'X
	tmp)))

(defun ISBN10->ISBN13-check-digits (list)
  "978系のみ対応する。979系のフランスと韓国には対応しない。"
  (let ((tmp (- 10 (mod (+ 38 (nth 1 list) (nth 3 list) (nth 5 list) (nth 7 list)
			   (* 3 (+ (nth 0 list) (nth 2 list) (nth 4 list) (nth 6 list) (nth 8 list)))) 10))))
    (if (= tmp 10)
	0
	tmp)))

(defun ISBN10->ISBN13 (isbn-string)
  (let ((string (ppcre:regex-replace-all "-" isbn-string "")))
    (let ((isbn-list (integer-strings->integers-list string))
	(isbn-rm-cd (subseq string 0 9)))
    (format nil "978~A~A" isbn-rm-cd
	    (isbn10->isbn13-check-digits isbn-list)))))

(defun ISBN13->ISBN10 (isbn-string)
  (let ((string (ppcre:regex-replace-all "-" isbn-string "")))
    (let ((isbn-list (integer-strings->integers-list string))
	(isbn-rm-etc (subseq string 3 12)))
    (format nil "~A~A" isbn-rm-etc
	    (isbn13->isbn10-check-digits isbn-list)))))

(defun ISBN-type (isbn-string)
  (let ((string-len (length isbn-string)))
    (cond ((= string-len 10) 10)
	  ((= string-len 13) 13)
	  (t (format nil "~A is not ISBN Code." isbn-string)))))

;; (defun eliminate-hyphen (string)
;;   (ppcre:regex-replace-all "-" string ""))

(defun ISBN? (isbn-string)
  (let ((string (eliminate-hyphen isbn-string)))
    (isbn-type string))) 


;;; for Session id
;;;
@export
(defun string->sha512-hex-string (string &key (prefix "G1:") (suffix ""))
  (concatenate
   'string
   prefix
   (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence
     :sha512
     (flexi-streams:string-to-octets string :external-format :utf8)))
   suffix))

@export
(defun random-string (&optional (length 1024))
  (coerce
   (loop :repeat length
	 :collect (let ((result nil))
		    (loop (if (not (null result)) (return result))
			  (setf result (code-char (mt19937:random 1114111))))))
   'string))

@export
(defun password+salt->sha512 (password salt &key (prefix "G1:") (suffix "") (stretch 20) (n 0))
  (let ((s password))
    (if (> n stretch)
	s
	(progn
	  (setf s (string->sha512-hex-string (concatenate 'string salt s) :prefix prefix :suffix suffix))
	  (password+salt->sha512 s salt :prefix prefix :suffix suffix :stretch stretch :n (1+ n))))))

;;; -*- end of file. -*-
