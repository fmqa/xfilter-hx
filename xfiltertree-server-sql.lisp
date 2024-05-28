(in-package :xfiltertree-server)

(defparameter *db* "db.sqlite3")

(defparameter *default-join*
  (list (cons '("event" . "endpoint")
              (eqvalg:conjunction-of
               (eqvalg:equality-of (eqvalg:column-of "egression" "egressor")
                                   (eqvalg:column-of "endpoint" "id"))
               (eqvalg:equality-of (eqvalg:column-of "egression" "egressed")
                                   (eqvalg:column-of "event" "id"))))))

(defun sql-log (statement parameters)
  (hunchentoot:log-message* :INFO "Performing SQL query: ~A~@[; with parameters: ~A~]" statement parameters))

(defun sql-query (statement &rest parameters)
  (sql-log statement parameters)
  (multiple-value-list
   (sqlite:with-open-database (db *db*)
     (apply #'sqlite:execute-one-row-m-v db statement parameters))))

(defun compute-aggregations (tree)
  (let ((eqvalg-sql:*join* *default-join*))
    (xfiltertree-sql:compute-aggregations tree #'sql-query)))

(defun sql-query-multi (statement &rest parameters)
  (sql-log statement parameters)
  (sqlite:with-open-database (db *db*)
    (apply #'sqlite:execute-to-list db statement parameters)))
