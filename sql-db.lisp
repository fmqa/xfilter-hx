(defpackage sql-db
  (:use :cl)
  (:export
   #:*pre-query-hook*
   #:call-with-db
   #:execute-one-row-m-v
   #:execute-to-list))
(in-package :sql-db)

(defparameter *db* "db.sqlite3")

(defparameter *pre-query-hook* nil "Pre-query hook")

(defun run-pre-query-hook (sql parameters)
  (when *pre-query-hook*
    (funcall *pre-query-hook* sql parameters)))

(defun call-with-db (f &rest arguments)
  (sqlite:with-open-database (db *db*)
    (let ((*db* db)) (apply f arguments))))

(defun execute-one-row-m-v (sql &rest parameters)
  (run-pre-query-hook sql parameters)
  (apply #'sqlite:execute-one-row-m-v *db* sql parameters))

(defun execute-to-list (sql &rest parameters)
  (run-pre-query-hook sql parameters)
  (apply #'sqlite:execute-to-list *db* sql parameters))
