(defpackage xfiltertree-sql
  (:use :cl)
  (:export
   #:*db*
   #:compute-aggregations
   #:endpoint-search
   #:endpoint-by-uri))
(in-package :xfiltertree-sql)

(defparameter *db* "db.sqlite3")

(defun call-with-db (f)
  (sqlite:with-open-database (db *db*)
    (funcall f db)))

(defun compute-aggregations (node-maker &optional constraints)
  (let* ((node (funcall node-maker))
         (node-aggregation-groups (xfiltertree-fql:constrain-tree-queries node constraints))
         (aggregation-groups (mapcar #'cdr node-aggregation-groups))
         (sql-setters
           (fql-sql:sqlize-aggregated-conjoined-group-list
            aggregation-groups))
         (sql-stmts (mapcar #'car sql-setters))
         (agg-setters (mapcar #'cdr sql-setters))
         (sql-q (format nil "SELECT ~{(~A)~^, ~}" sql-stmts))
         (sql-row (multiple-value-list
                   (call-with-db (lambda (db) (sqlite:execute-one-row-m-v db sql-q))))))
    (princ sql-q)
    (loop for (count . setter) in (mapcar #'cons sql-row agg-setters)
          do (funcall setter count))
    node))

(defun wordp (char)
  (or (alphanumericp char) (member char '(#\_ #\. #\: #\Space))))

(defun endpoint-search (text &optional (limit 200))
  (call-with-db
   (lambda (db)
     (sqlite:execute-to-list db "SELECT * FROM endpoint WHERE uri LIKE ? LIMIT ?"
                             (format nil "%~A%"
                                     (remove-if (lambda (c) (not (wordp c))) text))
                             limit))))

(defun endpoint-by-uri (uri)
  (call-with-db
   (lambda (db)
     (car (sqlite:execute-to-list db "SELECT * FROM endpoint WHERE uri = ?" uri)))))
