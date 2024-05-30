(defpackage sql-client
  (:use :cl)
  (:export
   #:*tap*
   #:query
   #:parametrized-query))
(in-package :sql-client)

(defparameter *tap* #'identity)

(defgeneric query (db sql)
  (:method :before (db sql)
    (funcall *tap* sql)))

(defun parametrized-query (db sql &rest parameters)
  (query db (cons sql parameters)))
