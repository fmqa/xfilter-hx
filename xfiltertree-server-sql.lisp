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

(defun sql-log (statement parameters)
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
  (eqvalg-sqlite:call-with-database
   (lambda ()
     (let ((eqvalg-sql:*join* *default-join*))
       (xfiltertree-eqvalg:compute-aggregations tree)))))

(defun populate-aggregations (tree)
  (eqvalg-sqlite:call-with-database
   (lambda ()
     (let ((eqvalg-sql:*join* *default-join*))
       (xfiltertree-eqvalg:populate-aggregations tree)))))
