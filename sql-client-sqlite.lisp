(defpackage sql-client-sqlite
  (:use :cl))
(in-package :sql-client-sqlite)

(defmethod sql-client:query ((db sqlite:sqlite-handle) sql)
  (if (listp sql)
      (apply #'sqlite:execute-to-list db sql)
      (sqlite:execute-to-list db sql)))
