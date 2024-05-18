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

(defun compute-aggregations (node &optional constraints)
  (let* ((tree (xfiltertree-fql:constrained-tree node constraints))
         (expression-aggregations (apply #'append (mapcar #'cdr tree)))
         (expressions (mapcar #'car expression-aggregations))
         (aggregations (mapcar #'cdr expression-aggregations))
         (sql-stmts (mapcar #'eqvalg-sql:sqlize-aggregation expressions))
         (sql-cmd (format nil "SELECT ~{(~A)~^, ~}" sql-stmts))
         (sql-row (multiple-value-list
                   (call-with-db (lambda (db) (sqlite:execute-one-row-m-v db sql-cmd))))))
    (princ sql-cmd)
    (loop for (count pair) in (mapcar #'cons sql-row aggregations)
          do (setf (cdr pair) count))
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
