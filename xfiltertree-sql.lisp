(defpackage xfiltertree-sql
  (:use :cl)
  (:export
   #:query-aggregation-tree
   #:*db*
   #:endpoint-search
   #:endpoint-by-uri))
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
  (format nil "WITH t AS (SELECT event.*, endpoint.* FROM event, endpoint, egression WHERE egression.egressor = endpoint.id AND egression.egressed = event.id AND (~A)), u AS (SELECT event.*, endpoint.* FROM event, endpoint, egression WHERE egression.egressor = endpoint.id AND egression.egressed = event.id AND (~A)) ~A"
          (sqlize-sorted-clauses (remove "event.type" clauses :key #'car :test #'equal))
          (sqlize-sorted-clauses (remove "event.connectionStatus" clauses :key #'car :test #'equal))
          *aggregations*))

(defun call-with-db (f)
  (sqlite:with-open-database (db *db*)
    (funcall f db)))

(defun query-db (clauses)
  (call-with-db
   (lambda (db)
     (sqlite:execute-one-row-m-v db (sqlize-aggregations clauses)))))

(defun build-tree (&rest values)
  (xfiltertree-bom:consume-aggregation-tree (lambda () (pop values))))

(defun query-aggregation-tree (clauses)
  (multiple-value-call #'build-tree (query-db clauses)))

(defun word-p (char)
  (or (alphanumericp char) (member char '(#\_ #\. #\: #\Space))))

(defun endpoint-search (text &optional (limit 200))
  (call-with-db
   (lambda (db)
     (sqlite:execute-to-list db "SELECT * FROM endpoint WHERE uri LIKE ? LIMIT ?"
                             (format nil "%~A%" (remove-if-not #'word-p text))
                             limit))))

(defun endpoint-by-uri (uri)
  (call-with-db
   (lambda (db)
     (car (sqlite:execute-to-list db "SELECT * FROM endpoint WHERE uri = ?" uri)))))
