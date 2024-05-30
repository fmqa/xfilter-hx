(in-package :xfiltertree-server)

(defparameter *db* "db.sqlite3")

(defparameter *default-join*
  (list (cons '("event" . "endpoint")
              (eqvalg:conjunction-of
               (eqvalg:equality-of (eqvalg:column-of "egression" "egressor")
                                   (eqvalg:column-of "endpoint" "id"))
               (eqvalg:equality-of (eqvalg:column-of "egression" "egressed")
                                   (eqvalg:column-of "event" "id"))))
        (cons '("event" . "marking")
              (eqvalg:equality-of (eqvalg:column-of "marking" "marked")
                                  (eqvalg:column-of "event" "id")))
        (cons '("endpoint" . "marking")
              (eqvalg:conjunction-of
               (eqvalg:equality-of (eqvalg:column-of "marking" "marked")
                                   (eqvalg:column-of "egression" "egressed"))
               (eqvalg:equality-of (eqvalg:column-of "egression" "egressor")
                                   (eqvalg:column-of "endpoint" "id"))))))

(defun sql-log (statement &optional parameters)
  (hunchentoot:log-message* :INFO "Performing SQL query: ~A~@[; with parameters: ~A~]" statement parameters))

(defun sql-query (statement &rest parameters)
  (sql-log statement parameters)
  (multiple-value-list
   (sqlite:with-open-database (db *db*)
     (apply #'sqlite:execute-one-row-m-v db statement parameters))))

(defun sql-query-multi (statement &rest parameters)
  (sql-log statement parameters)
  (sqlite:with-open-database (db *db*)
    (apply #'sqlite:execute-to-list db statement parameters)))

(defun compute-aggregations (tree)
  (sqlite:with-open-database (db *db*)
    (let ((eqvalg-sql:*join* *default-join*)
          (sql-client:*tap* #'sql-log))
      (xfiltertree-eqvalg:compute-aggregations tree (eqvalg-sql:cardinalities-in db)))))

(defun populate-aggregations (tree)
  (sqlite:with-open-database (db *db*)
    (let ((eqvalg-sql:*join* *default-join*)
          (sql-client:*tap* #'sql-log))
      (xfiltertree-eqvalg:populate-aggregations tree (eqvalg-sql:distinct-in db)))))
