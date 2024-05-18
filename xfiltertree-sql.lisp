(defpackage xfiltertree-sql
  (:use :cl)
  (:export
   #:query-aggregation-tree
   #:*db*))
(in-package :xfiltertree-sql)

(defparameter *aggregations*
  "SELECT
  (SELECT COUNT(*) FROM t WHERE type = 'PHONE_CALL') as eventTypePhoneCall,
  (SELECT COUNT(*) FROM t WHERE type = 'E_MAIL') as eventTypeEmail,
  (SELECT COUNT(*) FROM u WHERE type = 'PHONE_CALL' AND connectionStatus = 'ACCEPTED') as connectionStatusAccepted,
  (SELECT COUNT(*) FROM u WHERE type = 'PHONE_CALL' AND connectionStatus = 'REJECTED') as connectionStatusRejected")

(defparameter *db* "db.sqlite3")

(defun sqlize-all (attribute ops)
  (destructuring-bind (op . args) ops
    (ecase op
      (:eq (format nil "~A IN (~{'~A'~^,~})" attribute args))
      (:strict-eq (format nil "~A = '~A'" attribute (car args))))))

(defun sqlize-attribute (attribute modes)
  (loop with result = nil
        for (mode . ops) in modes
        do (loop for op in ops
                 do (ecase mode
                      (:all (push (sqlize-all attribute op) result))))
        finally (return result)))

(defun sqlize-sorted-clauses-where (clauses)
  (loop for (attribute . modes) in clauses
        appending (sqlize-attribute attribute modes)))

(defun sqlize-sorted-clauses (clauses)
  (if clauses
      (format nil "~{~A~^ AND ~}" (sqlize-sorted-clauses-where clauses))
      "1=1"))

(defun sqlize-aggregations (clauses)
  (format nil "WITH t AS (SELECT * FROM event WHERE ~A), u AS (SELECT * FROM event WHERE ~A) ~A"
          (sqlize-sorted-clauses (remove "event.type" clauses :key #'car :test #'equal))
          (sqlize-sorted-clauses (remove "event.connectionStatus" clauses :key #'car :test #'equal))
          *aggregations*))

(defun query-db (clauses)
  (sqlite:with-open-database (db *db*)
    (sqlite:execute-one-row-m-v db (sqlize-aggregations clauses))))

(defun build-tree (&rest values)
  (xfiltertree-bom:consume-aggregation-tree (lambda () (pop values))))

(defun query-aggregation-tree (clauses)
  (multiple-value-call #'build-tree (query-db clauses)))
