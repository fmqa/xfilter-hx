(in-package :xfiltertree-server)

(defun compute-aggregations (tree)
  (let ((eqvalg-sql:*join* *default-join*))
    (xfiltertree-eqvalg:compute-aggregations tree (eqvalg-sql:cardinalities-in #'default-query))))

(defun populate-aggregations (tree)
  (let ((eqvalg-sql:*join* *default-join*))
    (xfiltertree-eqvalg:populate-aggregations tree (eqvalg-sql:distinct-in #'default-query))))
