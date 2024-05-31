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

(defun call-with-database (thunk)
  (sqlite:with-open-database (db *db*)
    (funcall thunk db)))

(defun log-sql-query (statement &optional parameters)
  (hunchentoot:log-message* :INFO "Performing SQL query: ~A~@[; with parameters: ~A~]" statement parameters))

(let ((querier (sql-client:bind-parametrized-querier #'call-with-database :tap #'log-sql-query)))
  (defun default-parametrized-query () querier))

(let ((querier (sql-client:bind-querier #'call-with-database :tap #'log-sql-query)))
  (defun default-query () querier))

(defun compute-aggregations (tree)
  (let ((eqvalg-sql:*join* *default-join*))
    (xfiltertree-eqvalg:compute-aggregations tree (eqvalg-sql:cardinalities-in (default-query)))))

(defun populate-aggregations (tree)
  (let ((eqvalg-sql:*join* *default-join*))
    (xfiltertree-eqvalg:populate-aggregations tree (eqvalg-sql:distinct-in (default-query)))))
