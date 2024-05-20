(defpackage sql-db
  (:use :cl)
  (:export
   #:*intercept*
   #:call-with-db
   #:execute-one-row-m-v
   #:execute-to-list))
(in-package :sql-db)

(defparameter *db* "db.sqlite3")

(defparameter *intercept* nil "Query interception callback")

(defun call-with-db (f &rest arguments)
  (sqlite:with-open-database (db *db*)
    (let ((*db* db)) (apply f arguments))))

(defun execute-one-row-m-v (sql &rest parameters)
  (when *intercept* (funcall *intercept* sql parameters))
  (apply #'sqlite:execute-one-row-m-v *db* sql parameters))

(defun execute-to-list (sql &rest parameters)
  (when *intercept* (funcall *intercept* sql parameters))
  (apply #'sqlite:execute-to-list *db* sql parameters))
